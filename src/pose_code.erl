%% CDDL HEADER START    -*-Erlang-*-
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
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Intuitive module loader, used by {@link pose_command}.  Only loads
%% one module at a time.  Use {@link pose_command:load_command/1} to
%% load a command inclusive of any submodules.
%%
%% <ul>
%% <li> {@section Basic Load Process} </li>
%% <li> {@section Purge Handling} </li>
%% <li> {@section Warnings} </li>
%% <li> {@section Packaged Modules} </li>
%% <li> {@section Pose Namespace} </li>
%% </ul>
%%
%% ==Basic Load Process==
%%
%% Each Erlang module is treated as a potential executable command in `pose'.
%% A call to `pose_code:load_module/1` results in a search of the
%% directories listed on the current `PATH' environment variable, with a
%% twist:
%%
%% For each directory on `PATH' that ends in `ebin\', and for which the
%% current user has write access, `pose' will look for a parallel `src\'
%% directory, and if found, search for a matching `.erl' file therein.
%%
%% If an associated `.erl' file is found, and it is newer that the `.beam'
%% file, or if an `.erl' file is found for which no `.beam' file appears,
%% the `.erl' file will be compiled to its `ebin\' directory.  If this
%% compilation is successful, the module will be loaded.
%% Otherwise, an error is returned.
%%
%% If no associated `.erl' file is found, the `.beam' file on the `PATH'
%% is loaded.  If no `.beam' file is found, the search continues to the
%% next directory on `PATH', returning an error if no `.beam' file can be
%% found or compiled from source before the `PATH' is exhausted.
%%
%% ==Purge Handling==
%%
%% Whenever a new binary is obtained by `pose_code', a `code:soft_purge/1'
%% is called, and on a `true' result, current code for the binary is made
%% old (`code:delete/1') and the binary is loaded as current code.
%%
%% In the event of a `false' result from `code:soft_purge/1', a message is
%% broadcast to all active processes of the form
%% `{purging, PurgePid, Module}', where 'PurgePid' is the `pid()' of the
%% process initiating the purge, and 'Module' is the atom identifying the
%% module to be purged.
%%
%% In order to take advantage of this broadcast, and escape being killed
%% for lingering in old code, `pose'-compatible modules should begin with
%% a case clause in message loops to respond to `purging' messages with a
%% fully-qualified call to the loop function.  As per the following example:
%%
%% <pre>
%% loop(...) ->
%%   receive
%%     {purging, _Pid, _Mod} -> ?MODULE:loop(...);
%%                   *     *     *
%%   end.
%% </pre>
%%
%% ==Warnings==
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
%% compiled `fee.foo.fum' module would traditionally be sought at
%% `/home/user/project/ebin/fee/foo/fum.beam'.
%%
%% ==Pose Namespace==
%%
%% Unlike standard Erlang, `pose' looks for a module by unpackaged filename,
%% and upon finding such a file, loads it, returning the fully-qualified
%% packaged module name.  This means that `pose' would look for `fum' (per
%% our example above), as `/home/user/project/ebin/fum.beam', and then
%% upon successfully loading same, would return
%% <code>{module, 'fee.foo.fum'}</code>.
%%
%% Additionally, `pose' uses a `-package' directive to identify
%% `pose'-compatible files that have been compiled in the flat namespace
%% standard to Erlang and then recompile those files with a package
%% assigned by `pose' so as to ensure that each such package is uniqely
%% identified in the namespace of the currently running node.
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
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% TODO: module binary service (to avoid repetitive slurps)
%% TODO: conservative module loader (to preserve against collisions)

%% @version 0.1.9
-module(pose_code).
-version("0.1.9"). 

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported functions
%%

% used from within pose applications
-export([load/1, load_module/1, load_module/2]).
-export_type([load_err/0, load_warn/0]).

-compile({no_auto_import, [load_module/2]}).

%%
%% API functions
%%

-type command() :: pose:command().
-type load_warn() :: diff_path | flat_pkg.
-type error() :: atom() | {atom(), error()}.
-type load_err() :: {load, error()} | {slurp, error()} | error().
-type load_mod_rtn() :: {module, module()}
                        | {module, module(), load_warn()}
                        | {error, load_err()}.
-spec load(Command :: command()) -> load_mod_rtn().
%% @equiv load_module(Command)
%% @deprecated
load(Command) -> load_module(Command).

-spec load_module(Command :: command()) -> load_mod_rtn().
%% @doc Locate command on search path supplied by `PATH' environment
%% variable, compiling and loading updated module as necessary.
%% @end
%% @deprecated Initial PATH search should be done by pose_command.
load_module(Command) -> load_module(Command, pose:path()).

-type directory() :: file:filename().
-type search_path() :: [directory()].
-spec load_module(Command :: command(), Path :: search_path())
           -> load_mod_rtn().
%% @doc Locate command on search path supplied by `Path' parameter,
%% compiling and loading updated module as necessary.
%% @end
load_module(_Command, []) -> {error, notfound};
load_module(Command, Path) when is_atom(Command) ->
  load_module(atom_to_list(Command), Path);
load_module(Command, [Head | Tail]) ->  
  %?DEBUG("looking for ~s in ~s~n", [Command, Head]),
  case pose_compile:ensure_compiled(Command, Head) of
    {info, nobin}           -> load_module(Command, Tail);
    {info, _Info}           -> %?DEBUG("l: ~p~n", [Info]),
                               load_module(Command, Head, slurp);
    {ok, _Filename}         -> %?DEBUG("l: ~s~n", [Filename]),
                               load_module(Command, Head, slurp);
    {ok, Module, Binary}    -> %?DEBUG("l: ~p~n", [Module]),
                               load_module(Command, Head, Module, Binary);
    {error, What}           -> {error, {load, What}}
  end.

%%
%% Local functions
%%

%%%
% Load module
%%%

% Having found command, slurp binary from file.
load_module(Command, Dir, slurp) ->
  Filename = filename:join(Dir, string:concat(Command, ".beam")),
  case pose_beam:slurp_binary(Filename) of
    {ok, Module, Binary}	-> load_module(Command, Dir, Module, Binary);
    {error, What}			-> {error, {slurp, What}}
  end.

% Load new current module from binary.
load_module(Command, Dir, OrigModule, Binary) ->
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
  {ok, Version} = pose_beam:get_module_vsn(Binary),
  {ok, Package} = pose_beam:get_package(Binary),
  do_load(Cmd, Dir, Module, Binary, Version, Package).

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
do_load(Cmd, Dir, Module, Binary, _Version, Package, pack_true) ->
  BinFile = filename:join(Dir, string:concat(Cmd, ".beam")),
  case is_current(Module, Binary) of
    {false, Reason} -> ?DEBUG({reload, {Module, Reason}}),
                       commence_load(Module, BinFile, Binary, Package, Reason);
    true            -> {ok, Module}
  end.
                       
% Test if module has even been loaded.                       
is_current(Module, Beam) ->
  case code:is_loaded(Module) of
    false           -> {false, not_loaded};
    {file, _Loaded} -> is_current(Module, Beam, Module:module_info(compile))
  end.

% Test if module has different source path or vsn.
is_current(Module, Beam, CompileInfo) ->
  Erl = proplists:get_value(source, CompileInfo),
  SameSource = same_source(Erl, Beam),
  SameModVsn = same_vsn(Module, Beam),
  if SameSource == true, 
     SameModVsn == true         -> true;
     SameSource == true         -> SameModVsn;
     true                       -> SameSource
  end.

% Compare loaded source file with source file of beam.
same_source(Loaded, Beam) ->
  case pose_beam:get_source(Beam) of 
    {error, Reason}     -> {false, {diff_src, Reason}};
    {ok, undefined}     -> {false, {diff_src, undefined}};
    {ok, Loaded}        -> true;
    {ok, BeamSrc}       -> ?DEBUG({diff_src, Loaded, BeamSrc}),
                           {false, diff_src}
  end.

% Compare loaded module vsn with vsn of beam.
same_vsn(Module, Beam) ->
  LoadedVsn = proplists:get_value(vsn, Module:module_info(attributes)),
  case pose_beam:get_module_vsn(Beam) of
    {error, Reason}     -> {false, {diff_vsn, Reason}};
    {ok, undefined}     -> {false, {diff_vsn, undefined}};
    {ok, LoadedVsn}     -> true;
    {ok, BeamVsn}       -> ?DEBUG({diff_vsn, LoadedVsn, BeamVsn}),
                           {false, diff_vsn}
  end.

commence_load(Module, BinFile, Binary, Pkg, Why) ->  
  if Why /= not_loaded -> do_purge_delete(Module); true -> false end,
  case code:load_binary(Module, BinFile, Binary) of
    {error, What}       -> {error, {load, What}};
    {module, Module}    ->
      case Why of
        diff_path   -> {ok, Module, diff_path};
        _Else       -> if Pkg == '' -> {ok, Module, flat_pkg};
                          true      -> {ok, Module} end
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
  Head ! {purging, self(), Module},
  do_purge_delete(Module, Tail).

%%%
% Ensure packaged
%%%

% Force recompilation if explicit package of 'default'
ensure_packaged(Command, Dir, default) ->
  case pose_compile:ensure_compiled(Command, Dir, true) of
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