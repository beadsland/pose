%% CDDL HEADER START
%% -----------------------------------------------------------------------
%% The contents of this file are subject to the Common Development and
%% Distribution License, Version 1.0 (the "License"); you may not use
%% this file except in compliance with the License.  You should have
%% received a copy of the Common Development and Distribution License
%% along with this software.  If not, it can be retrieved online at
%% http://www.opensource.org/licenses/CDDL-1.0
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% When distributing Covered Code, include this CDDL Header Notice in
%% each file and include the License file at CDDL-LICENSE.  If applicable
%% add the following below the CDDL Header, with the fields enclosed
%% by brackets replaced by your own identifying information:
%% "Portions Copyright [year] [name of copyright owner]"
%%
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc This is a preliminary draft of the module loader for `nosh'.
%%
%% <b>Draft Notes:</b>
%%
%% Each Erlang module is treated as an executable in `nosh'.  When the
%% name of a module appears in first position on a `nosh' command line, a
%% matching `.beam' file is sought on each directory on the `PATH'
%% environment variable, with one modification:  For each directory on
%% `PATH' that ends in `ebin\', and for which the current user has write
%% access, `nosh' will look for a parallel `src\' directory, and if found,
%% search for a matching `.erl' file therein.
%%
%% If an associated `.erl' file is found, and it is newer that the `.beam'
%% file, or if an `.erl' file is found for which no `.beam' file appears,
%% the `.erl' file will be compiled to its `ebin\' directory.  If this
%% compilation is successful, the module will be loaded and evaluation
%% and execution proceeds.  Otherwise, the compiler error is written to
%% `stdout' and a non-zero status is returned.
%%
%% If no associated `.erl' file is found, the `.beam' file on the `PATH'
%% is loaded and evaluation and execution goes forward.  If no `.beam'
%% file is found, the search continues to the next directory on `PATH',
%% returning an error if no `.beam' file can be found or compiled from
%% source before the `PATH' is exhausted.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: module binary service (to avoid repetitive slurps)
%% TODO: conservative module loader (to preserve against collisions)

%% @version 0.1.3
-module(pose_code).
-version("0.1.3").

%%
%% Include files
%%

-define(debug, true).
-include("interface.hrl").

-include("macro.hrl").

-define(FILENAME(Path, Command, Extn), Path ++ "/" ++ Command ++ Extn).
-define(FILENAME(Path, Command), ?FILENAME(Path, Command, "")).

%%
%% Exported functions
%%

-export([run/2, test/1]).

%%
%% API functions
%%

%% Locate command on PATH, load from file if newer than currently loaded.
-type command() :: string() | atom().
-spec run(IO :: #std{}, Command :: command()) ->
        {module, module()} | {error, any()}.
%% @todo refactor as a no_return with all output on stdout/stderr
%% @todo get PATH from environment
run(IO, Command) when is_atom(Command) -> run(IO, atom_to_list(Command));
run(IO, Command) ->
  ?INIT_POSE,
  Path = [filename:absname("ebin"), filename:absname("deps/superl/ebin")],
  run(IO, Command, Path).

%% Test that we can throw appropriate warnings in various scenarios.
%% @deprecated
test(IO) ->
  ?DEBUG("Running ver. ~s nosh_load test.~n", [?VERSION(?MODULE)]),

  Root = filename:absname(""),

  {module, Mod1} = run(IO, "test", [?FILENAME(Root, "ebin/alt")]),
  Mod1:start(),
  {module, Mod2} = run(IO, "test", [?FILENAME(Root, "ebin/alt2")]),
  Mod2:start(),
  {module, Mod3} = run(IO, "test", [?FILENAME(Root, "ebin")]),
  Mod3:start(),
  test:start(),

  ?DEBUG("test: done~n").

%%
%% Local functions
%%

% Iterate over path list in search of command.
run(_IO, _Command, []) -> {error, notfound};
run(IO, Command, [Head | Tail]) ->
  case ensure_compiled(Command, Head) of
    {info, nobin}           -> run(IO, Command, Tail);
    {info, Info}            -> ?DEBUG("l: ~p~n", [Info]),
                               run(IO, Command, Head, slurp);
    {ok, Filename}			-> ?DEBUG("l: ~s~n", [Filename]),
                               run(IO, Command, Head, slurp);
    {ok, Module, Binary}	-> ?DEBUG("l: ~p~n", [Module]),
                               run(IO, Command, Head, Module, Binary);
    {error, What}			-> ?STDERR({load, What}),
                               {error, {load, What}}
  end.

% Having found command, slurp binary from file.
run(IO, Command, Dir, slurp) ->
  Filename = ?FILENAME(Dir, Command, ".beam"),
  case pose_beam:slurp_binary(Filename) of
    {ok, Module, Binary}	-> run(IO, Command, Dir, Module, Binary);
    {error, What}			-> {error, {slurp, What}}
  end.

% Load new current module from binary.
run(IO, Command, Dir, OrigModule, Binary) ->
  case run_load(Command, Dir, OrigModule, Binary) of
    {ok, Module, diff_path}	-> ?STDERR({Module, "namespace collision"}),
                               {module, Module};
    {ok, Module, flat_pkg} 	-> ?STDERR({Module, "flat package unsafe"}),
                               {module, Module};
    {ok, Module}			-> ?DEBUG("got module: ~p~n", [Module]),
                               {module, Module};
    {error, What}			-> {error, What}
  end.

%%%
% Run load
%%%

% Get version and package details from binary.
run_load(Cmd, Dir, Module, Binary) ->
  case pose_beam:get_binary_detail(Module, Binary) of
    {error, What}			->
      {error, {get_detail, What}};
    {ok, Version, Package}	->
      run_load(Cmd, Dir, Module, Binary, Version, Package)
  end.

% Make sure binary was compiled using any explicit package attribute.
run_load(Cmd, Dir, Module, Binary, Version, Package) ->
  case ensure_packaged(Cmd, Dir, Package) of
    {error, What}			                ->
      {error, {load, What}};
    {ok, NewMod, NewBin, NewVsn, NewPkg}	->
      run_load(Cmd, Dir, NewMod, NewBin, NewVsn, NewPkg, pack_true);
    ok				->
      run_load(Cmd, Dir, Module, Binary, Version, Package, pack_true)
  end.

% Make sure the binary is what is current in memory.
run_load(Cmd, Dir, Module, Binary, Version, Package, pack_true) ->
  Filename = ?FILENAME(Dir, Cmd, ".beam"),
  ensure_loaded(Module, Filename, Binary, Version, Package).

%%%
% Ensure loaded
%%%

% Check if module is currently loaded.
ensure_loaded(Module, BinFile, Bin, Vsn, Pkg) ->
  case code:is_loaded(Module) of
    {file, MemFile}	->
      ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, MemFile);
    false			->
      ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false)
  end.

% Figure out if new version of file needs to be loaded.
ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false) ->
  ensure_loaded(Module, BinFile, Bin, Vsn, Pkg, false, not_loaded);
ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile) ->
  MemVsn = ?ATTRIB(Module, vsn),
  if BinFile == MemFile, BinVsn == MemVsn	->
       {ok, Module};
     BinFile /= MemFile					  	->
       ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile, diff_path);
     BinVsn /= MemVsn						->
       ensure_loaded(Module, BinFile, Bin, BinVsn, Pkg, MemFile, diff_vsn)
  end.

% Load the new module version.
ensure_loaded(Module, BinFile, Bin, _BinVsn, Pkg, _MemFile, Why) ->
  if Why /= not_loaded -> do_purge_delete(Module); true -> false end,
  case code:load_binary(Module, BinFile, Bin) of
    {error, What}		->
      {error, {load, What}};
    {module, Module}	->
      case Why of
        not_loaded	-> if Pkg == '' -> {ok, Module, flat_pkg};
                          true		-> {ok, Module} end;
        diff_vsn	-> if Pkg == '' -> {ok, Module, flat_pkg};
                          true		-> {ok, Module} end;
        diff_path	-> {ok, Module, diff_path}
      end
  end.

%%%
% Do purge and delete
%%%

% Attempt a soft purge and then delete.
do_purge_delete(Module) ->
  case code:soft_purge(Module) of
    false 	-> do_purge_delete(Module, processes());
    true 	-> code:delete(Module)
  end.

% Broadcast that a hard purge is about to happen, then purge and delete.
do_purge_delete(Module, []) -> code:purge(Module), code:delete(Module);
do_purge_delete(Module, [Head | Tail]) ->
  Head ! {self(), purging, Module},
  do_purge_delete(Module, Tail).

%%%
% Ensure packaged
%%%

% Force recompilation if explicit package of 'default'
ensure_packaged(Command, Dir, default) ->
  case ensure_compiled(Command, Dir, true) of
    {ok, Module, Binary} 	->
      ensure_packaged(Command, Dir, Module, Binary);
    {error, What} 			->
      {error, {recompile, What}}
  end;
ensure_packaged(_Command, _Dir, _Package) -> ok.

% Return binary with details
ensure_packaged(_Command, _Dir, Module, Binary) ->
  case pose_beam:get_binary_detail(Module, Binary) of
    {error, What}			-> {error, {get_detail, What}};
    {ok, Version, Package}	-> {ok, Module, Binary, Version, Package}
  end.

%%%
% Ensure compiled
%%%

% By default, we don't force compilation.
ensure_compiled(Command, Path) -> ensure_compiled(Command, Path, false).

% Check if we can write to the ebin directory.
ensure_compiled(Cmd, Dir, Force) ->
  case pose_file:can_write(Dir) of
    {error, What}	-> {error, {file, What}};
    false			-> {info, readonly_dir};
    true			-> ensure_compiled(Cmd, Dir, Force, write_dir)
  end.

% Check if we can write to the beam file.
ensure_compiled(Cmd, Dir, Force, write_dir) ->
  Filename = ?FILENAME(Dir, Cmd, ".beam"),
  case pose_file:can_write(Filename) of
    {error, What}	-> {error, {file, What}};
    false			-> ensure_binary(Cmd, Dir, readonly);
    true			-> ensure_compiled(Cmd, Dir, Force, write_both)
  end;

% Find our source file, and if none found, confirm there is even a binary.
ensure_compiled(Cmd, BinDir, Force, write_both) ->
  case parallel_src(BinDir, Cmd) of
    nosrc					-> ensure_binary(Cmd, BinDir, nosrc);
    {ok, SrcDir, Project}	-> ensure_compiled(Cmd, BinDir, Force, SrcDir,
                                               Project)
  end.

% If we can't compile from source file, confirm we can use binary we have.
ensure_binary(Cmd, Dir, Why) ->
    HaveBinary = pose_file:can_read(?FILENAME(Dir, Cmd, ".beam")),
    if HaveBinary     -> {info, Why};
       true           -> {info, nobin}  % i.e., search next dir in path
    end.

% Get modification date of source file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj) ->
  SrcFile = ?FILENAME(SrcDir, Cmd, ".erl"),
  case pose_file:last_modified(SrcFile) of
    {error, What}	->
      {error, {file, What}};
    SrcMod			->
      ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod)
  end.

% Get modification date of binary file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod) ->
  BinFile = ?FILENAME(BinDir, Cmd, ".beam"),
  case pose_file:last_modified(BinFile) of
    {error, What}	->
      {error, {file, What}};
    BinMod 			->
      ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod, BinMod)
  end.

% Compare modification dates and compile if source is newer.
ensure_compiled(Cmd, BinDir, Force, SrcDir, Proj, SrcMod, BinMod) ->
  if SrcMod > BinMod; Force	-> do_compile(SrcDir, Cmd, Proj, BinDir);
     true				    -> {ok, ?FILENAME(BinDir, Cmd, ".beam")}
  end.

% Make sure we've got a directory to write to.
do_compile(SrcDir, Cmd, Project, BinDir)  ->
  case file:make_dir(BinDir) of
    ok				-> do_compile(SrcDir, Cmd, Project, BinDir, true_dir);
    {error, eexist} -> do_compile(SrcDir, Cmd, Project, BinDir, true_dir);
    {error, What}	-> {error, {What, BinDir}}
  end.

% Compile to a binary in memory.
do_compile(SrcDir, Cmd, Project, BinDir, true_dir) ->
  case get_otp_includes(BinDir) of
    {error, What}   -> {error, {includes, What}};
    {ok, InclList}  -> do_compile(SrcDir, Cmd, Project, BinDir, InclList)
  end;
do_compile(SrcDir, Cmd, Project, BinDir, InclList) ->
  case get_otp_package(BinDir) of
    {error, What}   -> {error, {package, What}};
    {ok, Package}   -> do_compile(SrcDir, Cmd, Project, BinDir, InclList,
                                  Package)
  end.

do_compile(SrcDir, Cmd, Project, BinDir, InclList, Package) ->
  Options = [verbose, warnings_as_errors, return_errors, binary,
            {d, package, Package}, {outdir, BinDir}] ++ InclList,
  Filename = ?FILENAME(SrcDir, Cmd, ".erl"),
  case compile:file(Filename, Options) of
    error						->
      {error, {compile, unspecified_error}};
    {error, Errors, Warnings}	->
      {error, {compile, {Errors, Warnings}}};
    {ok, ModuleName, Binary} 	->
      do_compile(SrcDir, Cmd, Project, BinDir, ModuleName, Package, Binary)
  end.

% Write our binary out to file.
do_compile(_SrcDir, Cmd, _Project, BinDir, ModuleName, _Package, Binary) ->
  Outfile = ?FILENAME(BinDir, Cmd, ".beam"),
  case file:write_file(Outfile, Binary) of
    {error, What}	-> {error, {What, Outfile}};
    ok				-> {ok, ModuleName, Binary}
  end.

%%%
% Get OTP compliant package
%%%

%% @todo make this chdir safe
get_otp_package(BinDir) ->
  ?DEBUG("get_otp_package(~p)~n", [BinDir]),
  AbsBin = filename:absname("ebin"),
  if BinDir == AbsBin ->
        "ebin";
     true                                ->
        Split = re:split(BinDir, "/", [{return, list}]),
        get_otp_package(BinDir, Split)
  end.

get_otp_package(_BinDir, []) -> {error, no_otp_path};
get_otp_package(BinDir, [Head | Tail]) ->
  if Head == "deps";
     Head == "apps" -> {ok, string:join(Tail, "/")};
     true           -> get_otp_package(BinDir, Tail)
  end.

%%%
% Get OTP standard include paths
%%%

get_otp_includes(BinDir) ->
  case pose_file:find_parallel_folder("ebin", "_temp_", BinDir) of
    {true, TempDir, _Project}   ->
      {ok, get_otp_includes(TempDir, ["deps", "apps"]) ++
        get_otp_includes("deps", ["deps"])};
    {false, BinDir}             ->
      {error, not_otp}
  end.

get_otp_includes(_TempDir, []) -> [];
get_otp_includes(TempDir, [Head | Tail]) ->
  Include = re:replace(TempDir, "_temp_.*$", Head, [{return, list}]),
  case pose_file:can_read(Include) of
    true            -> [{i, Include} | get_otp_includes(TempDir, Tail)];
    false           -> get_otp_includes(TempDir, Tail);
    {error, What}   -> {error, {file, What}}
  end.

%%%
% Find parallel source directory
%%%

% Find candidate src directory parallel to ebin.
 parallel_src(BinDir, Cmd) ->
  ?DEBUG("Seeking parallel src\n"),
  case pose_file:find_parallel_folder("ebin", "src", BinDir) of
    {true, SrcPath, Proj} -> parallel_src(BinDir, Cmd, SrcPath, Proj);
    _Else				  -> ?DEBUG("Didn't find parallel src\n"), nosrc
  end.

% Confirm it's readable and return result.
parallel_src(_BinDir, Command, SrcDir, Project) ->
  Filename = ?FILENAME(SrcDir, Command, ".erl"),
  case pose_file:can_read(Filename) of
    true 	-> {ok, SrcDir, Project};
    false 	-> nosrc
  end.