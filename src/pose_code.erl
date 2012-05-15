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

%% @doc Intuitive module compiler and loader.
%%
%% <ul>
%% <li> {@section Basic Load Process} </li>
%% <li> {@section Packaged Modules} </li>
%% </ul>
%%
%% ==Basic Load Process==
%%
%% Each Erlang module is treated as a potential executable command in `pose'.
%% A call to `pose_code:load/1` results in a search of the directories
%% listed on the current `PATH' environment variable, with a twist:
%%
%% For each directory on
%% `PATH' that ends in `ebin\', and for which the current user has write
%% access, `pose' will look for a parallel `src\' directory, and if found,
%% search for a matching `.erl' file therein.
%%
%% If an associated `.erl' file is found, and it is newer that the `.beam'
%% file, or if an `.erl' file is found for which no `.beam' file appears,
%% the `.erl' file will be compiled to its `ebin\' directory.  If this
%% compilation is successful, the module will be loaded.
%% Otherwise, an error is returned.
%%
%% If no associated `.erl' file is found, the `.beam' file on the `PATH'
%% is loaded and evaluation and execution goes forward.  If no `.beam'
%% file is found, the search continues to the next directory on `PATH',
%% returning an error if no `.beam' file can be found or compiled from
%% source before the `PATH' is exhausted.
%%
%% ===Warnings===
%%
%% Load may return successfully with either a 2-tuple, `{module, Module}'
%% or a 3-tuple `{module, Module, Warning}'.  In the later case, the
%% `Warning' may be either of:
%%
%% <table>
%% <tr><td> `flat_pkg' </td>
%% <td> The module was compiled under Erlang's flat namespace, and no
%%      `-package' directive was found indicating that `pose' could
%%      recompile the module under the {@section Pose Namespace}.
%%      Flat-package modules are considered unsafe, as there may be
%%      other module binaries or source files with the same name elsewhere
%%      in the file system.</td></tr>
%% <tr><td> `diff_path' </td>
%% <td> The module was compiled under a namespace that matches the namespace
%%      of old code loaded from a different file path.  That is, the current
%%      and old code in the system originate from different points in the
%%      file system.  Such a <i>namespace collision</i> can occur when
%%      flat-package modules with the same name are loaded from different
%%      points in the file system.</td></tr>
%% </table>
%%
%% ==Packaged Modules==
%%
%% ===Introduction===
%%
%% Erlang provides for namespace management through an experimental
%% packages feature.  As implemented in Erlang, the package of a module
%% is expressed as a dot-separated path in the `-module' directive.
%% For instance, a package `fum' in the `fee.foo' package (where `fee.foo'
%% is a subpackage of `fee'), would be declared as:
%%
%% <pre>
%% -module(fee.foo.fum).
%% </pre>
%%
%% The package hierarchy, in turn, corresponds to the file hierarchy of
%% a module relative to the current code path.  So, continuing our example,
%% if the current code path includes `/home/user/project/ebin', the
%% compiled `fee.foo.fum' module would be sought at
%% `/home/user/project/ebin/fee/foo/fum.beam'.
%%
%% ===Pose Namespace===
%%
%% Unlike standard Erlang, `pose' looks for a module by unpackaged filename,
%% and upon finding such a file, loads it, returning the fully-qualified
%% packaged module name.  This means that `pose' would look for `fum' (per
%% our example above), as `/home/user/project/ebin/fum.beam', and then upon
%% successfully loading same, would return <code>{module, 'fee.foo.fum'}</code>.
%%
%% Additionally, `pose' uses a `-package' directive to identify files that
%% have been compiled in the flat namespace standard to Erlang and
%% then recompile those files with a package assigned by `pose' so as to
%% ensure that each such package is uniqely identified in the namespace
%% of the currently running node.
%%
%% Users can take advantage of the `-package' directive by including the
%% following pattern in their `pose'-compatible modules.
%%
%% <pre>
%% -define(module, fum).
%%
%% % BEGIN POSE PACKAGE PATTERN
%% -ifndef(package).
%% -module(?module).
%% -package(default).
%% -else.
%% -module(?package.?module).
%% -package(?package).
%% -endif.
%% % END POSE PACKAGE PATTERN
%% </pre>
%%
%% When `pose' sees that a module has been compiled with a `-package'
%% attribute of `default', it recompiles the module with the macro `?package'
%% set to a path unique to that module and the other modules in the same
%% directory.
%%
%% This allows modules to be developed in the flat namespace recognized by
%% all existing Erlang development tools, while ensuring that those same
%% modules will run in their own unique namespace when loaded in a
%% `pose'-compatible system.
%% @end
%% @reference See discussion of
%% <a href="http://www.erlang.se/publications/packages.html">Packages in
%% Erlang</a>.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% TODO: module binary service (to avoid repetitive slurps)
%% TODO: conservative module loader (to preserve against collisions)

%% @version 0.1.5
-module(pose_code).
-version("0.1.5").

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

% used from within pose applications
-export([load/1]).
-export_type([load_err/0]).

%%
%% API functions
%%

%% Locate command on `PATH', compiling and loading updated module as necessary.
-type command() :: pose:command().
-type load_warn() :: diff_path | flat_pkg.
-type error() :: atom() | {atom(), error()}.
-type load_err() :: {load, error()} | {slurp, error()} | error().
-type load_rtn() :: {module, module()} | {module, module(), load_warn()}
                    | {error, load_err()}.
-spec load(Command :: command()) -> load_rtn().
%% @todo refactor as a no_return with all output on stdout/stderr
%% @todo get PATH from environment
load(Command) when is_atom(Command) -> load(atom_to_list(Command));
load(Command) ->
  Path = [filename:absname("ebin"),
          filename:absname("deps/superl/ebin"),
          filename:absname("deps/bin/ebin"),
          filename:absname("deps/erl/ebin"),
          filename:absname("deps/nosql/ebin")],
  load(Command, Path).

%%
%% Local functions
%%


%%%
% Load command
%%%

% Iterate over path list in search of command.
load(_Command, []) -> {error, notfound};
load(Command, [Head | Tail]) ->
  ?DEBUG("looking for ~s in ~s~n", [Command, Head]),
  case ensure_compiled(Command, Head) of
    {info, nobin}           -> load(Command, Tail);
    {info, Info}            -> ?DEBUG("l: ~p~n", [Info]),
                               load(Command, Head, slurp);
    {ok, Filename}			-> ?DEBUG("l: ~s~n", [Filename]),
                               load(Command, Head, slurp);
    {ok, Module, Binary}	-> ?DEBUG("l: ~p~n", [Module]),
                               load(Command, Head, Module, Binary);
    {error, What}			-> {error, {load, What}}
  end.

% Having found command, slurp binary from file.
load(Command, Dir, slurp) ->
  Filename = ?FILENAME(Dir, Command, ".beam"),
  case pose_beam:slurp_binary(Filename) of
    {ok, Module, Binary}	-> load(Command, Dir, Module, Binary);
    {error, What}			-> {error, {slurp, What}}
  end.

% Load new current module from binary.
load(Command, Dir, OrigModule, Binary) ->
  case do_load(Command, Dir, OrigModule, Binary) of
    {ok, Module, diff_path}	-> {module, Module, diff_path};
    {ok, Module, flat_pkg} 	-> {module, Module, flat_pkg};
    {ok, Module}			-> ?DEBUG("got module: ~p~n", [Module]),
                               {module, Module};
    {error, What}			-> {error, What}
  end.

%%%
% Run load
%%%

% Get version and package details from binary.
do_load(Cmd, Dir, Module, Binary) ->
  case pose_beam:get_binary_detail(Module, Binary) of
    {error, What}			->
      {error, {get_detail, What}};
    {ok, Version, Package}	->
      do_load(Cmd, Dir, Module, Binary, Version, Package)
  end.

% Make sure binary was compiled using any explicit package attribute.
do_load(Cmd, Dir, Module, Binary, Version, Package) ->
  case ensure_packaged(Cmd, Dir, Package) of
    {error, What}			                ->
      {error, {load, What}};
    {ok, NewMod, NewBin, NewVsn, NewPkg}	->
      do_load(Cmd, Dir, NewMod, NewBin, NewVsn, NewPkg, pack_true);
    ok				->
      do_load(Cmd, Dir, Module, Binary, Version, Package, pack_true)
  end.

% Make sure the binary is what is current in memory.
do_load(Cmd, Dir, Module, Binary, Version, Package, pack_true) ->
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

% Find any source file, and get modification date of same.
ensure_compiled(Cmd, BinDir, Force, write_both) ->
  case parallel_src(BinDir, Cmd) of
    nosrc			-> ensure_binary(Cmd, BinDir, nosrc);
    {ok, SrcDir}	-> ensure_compiled(Cmd, BinDir, Force, SrcDir)
  end;
ensure_compiled(Cmd, BinDir, Force, SrcDir) ->
  SrcFile = ?FILENAME(SrcDir, Cmd, ".erl"),
  case pose_file:last_modified(SrcFile) of
    {error, What}   ->
      {error, {file, What}};
    SrcMod          ->
      ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod)
  end.

% If we can't compile from source file, confirm we can use binary we have.
ensure_binary(Cmd, Dir, Why) ->
    HaveBinary = pose_file:can_read(?FILENAME(Dir, Cmd, ".beam")),
    if HaveBinary     -> {info, Why};
       true           -> {info, nobin}  % i.e., search next dir in path
    end.

% Get modification date of binary file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod) ->
  BinFile = ?FILENAME(BinDir, Cmd, ".beam"),
  case pose_file:last_modified(BinFile) of
    {error, What}	->
      {error, {file, What}};
    BinMod 			->
      ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod, BinMod)
  end.

% Compare modification dates and compile if source is newer.
ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod, BinMod) ->
  if SrcMod > BinMod; Force	-> do_compile(SrcDir, Cmd, BinDir);
     true				    -> {ok, ?FILENAME(BinDir, Cmd, ".beam")}
  end.

% Make sure we've got a directory to write to.
do_compile(SrcDir, Cmd, BinDir)  ->
  case file:make_dir(BinDir) of
    ok				-> do_compile(SrcDir, Cmd, BinDir, true_dir);
    {error, eexist} -> do_compile(SrcDir, Cmd, BinDir, true_dir);
    {error, What}	-> {error, {What, BinDir}}
  end.

% Compile to a binary in memory.
do_compile(SrcDir, Cmd, BinDir, true_dir) ->
  case get_otp_includes(BinDir) of
    {error, What}   -> {error, {includes, What}};
    {ok, InclList}  -> do_compile(SrcDir, Cmd, BinDir, InclList)
  end;
do_compile(SrcDir, Cmd, BinDir, InclList) ->
  case get_otp_package(BinDir) of
    {error, What}   -> {error, {package, What}};
    {ok, Package}   -> do_compile(SrcDir, Cmd, BinDir, InclList, Package)
  end.

do_compile(SrcDir, Cmd, BinDir, InclList, Package) ->
  Options = [verbose, warnings_as_errors, return_errors, binary,
            {d, package, Package}, {outdir, BinDir}] ++ InclList,
  Filename = ?FILENAME(SrcDir, Cmd, ".erl"),
  case compile:file(Filename, Options) of
    error						->
      {error, {compile, unspecified_error}};
    {error, Errors, Warnings}	->
      {error, {compile, {Errors, Warnings}}};
    {ok, ModuleName, Binary} 	->
      do_compile(SrcDir, Cmd, BinDir, ModuleName, Package, Binary)
  end.

% Write our binary out to file.
do_compile(_SrcDir, Cmd, BinDir, ModuleName, _Package, Binary) ->
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
  Pwd = filename:absname(""),
  {ok, MP} = re:compile("^" ++ Pwd ++ "/(.*)$"),
  case re:run(BinDir, MP, [{capture, [1], list}]) of
    nomatch         -> {error, off_pwd};
    {match, [Path]} -> get_otp_package(BinDir, Path)
  end.

get_otp_package(_BinDir, Path) ->
  Package = re:replace(Path, "\/", ".", [{return, list}, global]),
  ?DEBUG("package: ~s~n", [Package]),
  {ok, list_to_atom(Package)}.


%%%
% Get OTP standard include paths
%%%

get_otp_includes(BinDir) ->
  case pose_file:find_parallel_folder("ebin", "_temp_", BinDir) of
    {true, TempDir}   ->
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
    {true, SrcPath} -> parallel_src(BinDir, Cmd, SrcPath);
    _Else	        -> ?DEBUG("Didn't find parallel src\n"), nosrc
  end.

% Confirm it's readable and return result.
parallel_src(_BinDir, Command, SrcDir) ->
  Filename = ?FILENAME(SrcDir, Command, ".erl"),
  case pose_file:can_read(Filename) of
    true 	-> {ok, SrcDir};
    false 	-> nosrc
  end.