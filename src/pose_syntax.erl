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
%% Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Erlang syntax utility functions.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.1.1
-module(pose_syntax).
-version("0.1.1").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

% Generic API entry point

-export([harvest/2]).

% Type-specific API entry points.

-export([qualifiers/1]).

%%
%% API Functions
%%

-type syntax_type() :: atom().
-type filename() :: file:filename().
-type node_data() :: term().
-spec harvest(Type :: syntax_type(), File :: filename()) -> [node_data()].
%% @doc List all items of a specific type in an Erlang source file.
harvest(Type, File) -> 
  {ok, Forms} = epp_dodger:parse_file(File, [{no_fail, true}, {clever, true}]),
  harvest(Type, Forms, []).

% Traverse list of syntax trees, extracting data of each node of specified type.
harvest(_Type, [], Picks) -> Picks;
harvest(Type, [Head | Tail], Picks) ->
  case erl_syntax:type(Head) of
    Type -> NewPicks = [erl_syntax:data(Head) | Picks];
    T    -> NewPicks = Picks
  end,
  harvest(Type, lists:flatten(erl_syntax:subtrees(Head)) ++ Tail, NewPicks).

-spec qualifiers(File :: filename()) -> [module()].
%% @doc Uniquely list all modules that qualify function calls.
qualifiers(File) -> qualifiers(harvest(module_qualifier, File), sets:new()).

% Extract module name from qualifier structure.
qualifiers([], Set) -> sets:to_list(Set);
qualifiers([{module_qualifier, {atom, _, Module}, _} | Tail], Set) ->
  qualifiers(Tail, sets:add_element(Module, Set));
qualifiers([_Head | Tail], Set) -> qualifiers(Tail, Set).

%%
%% Local Functions
%%