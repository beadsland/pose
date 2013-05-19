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

%% @version 0.1.7
-module(pose_command).
-version("0.1.7").

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
-export_type([load_mod_warn/0]).

%%
%% API Functions
%%

-type load_warn() :: pose_code:load_warn().
-type load_err() :: pose_code:load_err().
-type load_mod_warn() :: {module(), load_warn()} | load_warn().
-type load_cmd_rtn() :: {module, module(), [load_mod_warn()]}
                        | {error, load_err(), [load_mod_warn()]}.
-spec load(Command :: pose:command()) -> load_cmd_rtn().
%% @equiv load_command(Command)
load(Command) -> load_command(Command).

-spec load_command(Command :: pose:command()) -> load_cmd_rtn().
% @doc Load a command module and all submodules in the same directory.
% Here, a submodule is indicated by the syntax
% <code><i>module</i>_<i>subpart</i></code>.
% @end
load_command(Command) ->
  ?DEBUG("Pose loading command ~p~n", [Command]),
  case pose_code:load_module(Command) of
    {module, Module, Warning} -> load_command(Command, Module, [Warning]);
    {module, Module}          -> load_command(Command, Module, []);
    {error, What}             -> {error, What, []}
  end.

%%
%% Local Functions
%%

%%%
% Load command
%%%

% Determine if we can refer to a parallel source folder.
%% @todo get PATH from environment
%% @todo add to PATH from erl commandline
load_command(Command, Module, Warnings) ->
  %?BAIL(Module:module_info()),
  BinPath = filename:dirname(code:which(Module)),
  ?DEBUG({binpath, BinPath}),
  case pose_file:find_parallel_folder("ebin", "src", BinPath) of
    {true, SrcPath}     ->
      SubModList = get_submodule_list(Command, BinPath, {srcpath, SrcPath});
    {false, BinPath}    ->
      SubModList = get_submodule_list(Command, BinPath, ".beam")
  end,
  ?DEBUG("submodule list: ~p~n", [SubModList]),
  load_command(Command, Module, BinPath, Warnings, SubModList).

% List submodules, in source folder, if readable, or else in binaries folder.
get_submodule_list(Command, BinPath, Data) when is_atom(Command) ->
  get_submodule_list(atom_to_list(Command), BinPath, Data);
get_submodule_list(Command, BinPath, {srcpath, SrcPath}) ->
  case pose_file:can_read(SrcPath) of
    true            -> get_submodule_list(Command, SrcPath, ".erl");
    false           -> get_submodule_list(Command, BinPath, ".beam");
    {error, _What}  -> get_submodule_list(Command, BinPath, ".beam")
  end;
get_submodule_list(Command, Path, Extension) ->
  Pattern = lists:append([Path, "/", Command, "_*", Extension]),
  ?DEBUG({submodule_pattern, Pattern}),
  WildList = filelib:wildcard(Pattern),
  [get_submodule_subpattern(X) || X <- WildList].

% Predicate function for get_submodule_list/3 list comprehension.
get_submodule_subpattern(File) ->
  {ok, MP} = re:compile("^.*/([^/]+)\\.[beamrl]+$"),
  Options = [{capture, [1], list}],
  {match, [Module]} = re:run(File, MP, Options),
  list_to_atom(Module).

% Load each submodule, appending to warnings list as necessary.
load_command(_Command, Module, _BinPath, Warnings, []) ->
  {module, Module, Warnings};
load_command(Command, Module, BinPath, Warnings, [Head | Tail]) ->
  case pose_code:load_module(Head, [BinPath]) of
    {module, _SubModule, NewWarn}   ->
      UpdatedWarnings = [{Head, NewWarn} | Warnings],
      load_command(Command, Module, BinPath, UpdatedWarnings, Tail);
    {module, _SubModule}            ->
      load_command(Command, Module, BinPath, Warnings, Tail);
    {error, What}                   ->
      {error, {Head, What}, Warnings}
  end.