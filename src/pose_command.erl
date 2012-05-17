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

%% @doc Load command modules and submodules in a single operation.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.7
-module(pose_command).
-version("0.1.7").

%%
%% Include files
%%

%-define(debug, true).
-include("interface.hrl").

-include("macro.hrl").

-compile({no_auto_import, [load_module/2]}).

%%
%% Exported Functions
%%

-export([load/1, load_command/1]).
-export_type([load_mod_warn/0]).

%%
%% API Functions
%%

-type load_warn() :: pose_code:load_warn().
-type load_mod_warn() :: {module(), load_warn()} | load_warn().
-type load_cmd_rtn() :: pose_code:load_mod_rtn()
                        | {module, module(), [load_mod_warn()]}.
-spec load(Command :: pose:command()) -> load_cmd_rtn().
%% @equiv load_command(Command)
load(Command) -> load_command(Command).

-spec load_command(Command :: pose:command()) -> load_cmd_rtn().
% @doc Load a command module and all submodules in the same directory.
% Here, a submodule is indicated by the syntax
% <code><i>module</i>_<i>subpart</i></code>.
% @end
load_command(Command) ->
  case pose_code:load_module(Command) of
    {module, Module, Warning} -> load_command(Command, Module, [Warning]);
    {module, Module}          -> load_command(Command, Module, []);
    {error, What}             -> {error, What}
  end.

%%
%% Local Functions
%%

%%%
% Load command
%%%

% Determine if we can refer to a parallel source folder.
load_command(Command, Module, Warnings) ->
  BinPath = filename:dirname(code:which(Module)),
  case pose_file:find_parallel_folder("ebin", "src", BinPath) of
    {true, SrcPath}     ->
      SubModList = get_submodule_list(Command, BinPath, {srcpath, SrcPath});
    {false, BinPath}    ->
      SubModList = get_submodule_list(Command, BinPath, ".beam")
  end,
  load_command(Command, Module, BinPath, Warnings, SubModList).

% List submodules, in source folder, if readable, or else in binaries folder.
get_submodule_list(Command, BinPath, {srcpath, SrcPath}) ->
  case pose_file:can_read(SrcPath) of
    true            -> get_submodule_list(Command, SrcPath, ".erl");
    false           -> get_submodule_list(Command, BinPath, ".beam");
    {error, _What}  -> get_submodule_list(Command, BinPath, ".beam")
  end;
get_submodule_list(Command, Path, Extension) ->
  Pattern = lists:append([Path, "/", Command, "_*.", Extension]),
  WildList = filelib:wildcard(Pattern),
  [get_submodule_subpattern(X) || X <- WildList].

% Predicate function for get_submodule_list/3 list comprehension.
get_submodule_subpattern(File) ->
  {ok, MP} = re:compile("^.*/([^/]*)\.[beamrl]+$"),
  Options = [{capture, [1], list}],
  {match, [Module]} = re:run(File, MP, Options),
  list_to_atom(Module).

% Load each submodule, appending to warnings list as necessary.
load_command(_Command, Module, _BinPath, Warnings, []) ->
  if Warnings == [] -> {module, Module};
     true           -> {module, Module, Warnings}
  end;
load_command(Command, Module, BinPath, Warnings, [Head | Tail]) ->
  case pose_code:load_module(Head, [BinPath]) of
    {module, Module, NewWarn}   ->
      UpdatedWarnings = [{Head, NewWarn} | Warnings],
      load_command(Command, Module, BinPath, UpdatedWarnings, Tail);
    {module, Module}            ->
      load_command(Command, Module, BinPath, Warnings, Tail);
    {error, What}               ->
      {error, {Head, What}}
  end.