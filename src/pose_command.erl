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

%% @doc Load command modules and submodules in a single operation.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @version 0.1.8
-module(pose_command).
-version("0.1.8").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

-export([load/1, load_command/1]).

%%
%% API Functions
%%

-type load_warnings() :: [pose_code:load_warn()].
-type load_result() :: {module, module(), load_warnings()}.
-type load_err_term() :: pose_code:load_err() | nopath | {submodules, nopath}.
-type load_error() :: {error, load_err_term(), load_warnings()}.
-type load_return() :: load_result() | load_error().
-spec load(Command :: pose:command()) -> load_return().
%% @equiv load_command(Command)
load(Command) -> load_command(Command).

-spec load_command(Command :: pose:command()) -> load_return().
% @doc Load a command module and all submodules in the same directory.
% Here, a submodule is indicated by the syntax
% <code><i>module</i>_<i>subpart</i></code>.
% @end
load_command(Command) when is_atom(Command) -> 
  load_command(atom_to_list(Command));
load_command(Command) ->
  ?DEBUG("Pose loading command ~s~n", [Command]),
  case pose_code:load_module(Command, pose:path()) of
    {module, Module, Warning} -> load_command(Command, Module, [Warning]);
    {module, Module}          -> load_command(Command, Module, []);
    {error, Reason}           -> {error, Reason, []}
  end.

%%
%% Local Functions
%%

%%%
% Load command
%%%

% Get the folder where submodules of our command are to be found.
load_command(Command, Module, Warnings) ->
  case srcpath(Module) of 
    undefined -> EbinPath = ebinpath(Module),
                 load_command(Command, Module, Warnings, EbinPath, ".beam");
    SrcPath   -> load_command(Command, Module, Warnings, SrcPath, ".erl")
  end.

% Find any submodules of command that are to be loaded.
load_command(_Command, _Module, Warnings, undefined, _Extn) -> 
  {error, nopath, Warnings};
load_command(Command, Module, Warnings, Path, Extn) ->
  BasePattern = string:concat(Command, string:concat("_*", Extn)),
  Pattern = filename:join(Path, BasePattern),
  SubMods = [get_submodule_subpattern(X) || X <- filelib:wildcard(Pattern)],
  case pose_file:find_parallel_folder("src", "ebin", Path) of
    {false, _}       -> {error, {submodules, nopath}};
    {true, EbinPath} -> load_submodules(Module, EbinPath, Warnings, SubMods)
  end.

% Load each submodule, appending to warnings list as necessary.
load_submodules(Module, _Path, Warnings, []) -> {module, Module, Warnings};
load_submodules(Module, Path, Warnings, [Head | Tail]) ->  
  %?DEBUG("Pose loading submodule ~s~n", [Head]),  
  case pose_code:load_module(Head, [Path]) of
    {module, _SubModule, NewWarn}   ->
      UpdatedWarnings = [{Head, NewWarn} | Warnings],
      load_submodules(Module, Path, UpdatedWarnings, Tail);
    {module, _SubModule}            ->
      load_submodules(Module, Path, Warnings, Tail);
    {error, Reason}                 ->
      {error, {Head, Reason}, Warnings}
  end.

% Predicate function for get_submodule_list list comprehension.
get_submodule_subpattern(File) ->
  {ok, MP} = re:compile("^.*/([^/]+)\\.[beamrl]+$"),
  Options = [{capture, [1], list}],
  {match, [Module]} = re:run(File, MP, Options),
  list_to_atom(Module).

% Try to determine the source directory of a module.
srcpath(Module) ->
  case srcinfo(Module) of 
    undefined -> srcpath(Module, ebinpath(Module));
    Source    -> filename:dirname(Source)
  end.

% Look for source directory as parallel of binary directory.
srcpath(_Module, undefined) -> undefined;
srcpath(_Module, EbinPath) ->
  case pose_file:find_parallel_folder("ebin", "src", EbinPath) of
    {false, _}      -> undefined;
    {true, SrcPath} -> SrcPath
  end.

% Get source file from compile info, unless it was stripped.
srcinfo(Module) -> proplists:get_value(source, Module:module_info(compile)). 

% Determine the binary directory of the module.
ebinpath(Module) ->
  case code:is_loaded(Module) of
    false                           -> undefined;
    {file, Atom} when is_atom(Atom) -> ebinpath(Module, srcinfo(Module));
    {file, File}                    -> ebinpath(Module, File)
  end.

% Confirm we've got a binary path, or find it as parallel to a source path.
ebinpath(_Module, undefined) -> undefined;
ebinpath(Module, File) ->
  case filename:extension(File) of
    []      -> undefined;
    ".beam" -> filename:dirname(File);
    ".erl"  -> Dir = filename:dirname(File),
               Parallel = pose_file:find_parallel_folder("src", "ebin", Dir),
               ebinpath(Module, File, Parallel)
  end.

ebinpath(_Module, _File, {false, _}) -> undefined;
ebinpath(_Module, _File, {true, Path}) -> Path.