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

%% @doc Entry points for running `pose'-compatible commands.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.7
-module(pose).
-version("0.1.7").

%%
%% Include files
%%

%-define(debug, true).
-include("interface.hrl").

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Hidden callbacks
-export([do_run/2]).

% Internal entry functions
-compile({no_auto_import, [spawn/2, spawn/3, spawn/4]}).
-export([spawn/2, spawn/3]).
-export_type([command/0]).

% pose_command helper function
-export([send_load_warnings/3]).

% hidden functions
-export([argv/2]).

%%
%% gen_command functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% API Functions
%%

do_run(IO, PoseARG) ->
  io:format("Starting pose ~p~n", [self()]),
  [Command | Param] = PoseARG#arg.v,
  ARG = ?ARG(Command, Param),
  case pose_command:load(Command) of
    {module, Module, Warnings}    ->
      pose:send_load_warnings(IO, superl, Warnings),
      Module:run(IO, ARG, ?ENV);
    {error, What, Warnings}       ->
      pose:send_load_warnings(IO, superl, Warnings),
      ?STDERR({Command, What}),
      exit(What)
  end.

-type command() :: nonempty_string() | atom().
-type spawn_rtn() :: {error, pose_code:load_err()} | pid().
-spec spawn(IO :: #std{}, Command :: command()) -> spawn_rtn().
%% @equiv spawn(IO, Command, [])
%% @deprecated Only nosh uses this
spawn(IO, Command) -> spawn(IO, Command, []).

-spec spawn(IO :: #std{}, Command :: command(), Param :: [any()]) ->
        spawn_rtn().
%% @doc Run a pose-compliant command in its own process.
%% @deprecated
spawn(IO, Command, Param) when is_atom(Command) ->
  ?MODULE:spawn(IO, atom_to_list(Command), Param);
  % Fully qualified call to satisfy dialyzer,
  % which otherwise doesn't see spawn/4 ever run.
spawn(IO, Command, Param) ->
  case pose_command:load(Command) of
    {module, Module, Warnings}  ->
      send_load_warnings(IO, Command, Warnings),
      spawn(IO, Command, Param, Module);
    {error, Else, Warnings}     ->
      send_load_warnings(IO, Command, Warnings),
      {error, Else}
  end.

-type warning() :: pose_command:load_mod_warn().
-spec send_load_warnings(IO :: #std{}, Command :: command(),
                         Warnings :: [warning()]) -> ok.
%% @doc Send messages to `stderr' process detailing any warnings received
%% from `pose_command:load/1'.
send_load_warnings(_IO, _Command, []) -> ok;
send_load_warnings(IO, Command, Warnings) when is_atom(Command) ->
  send_load_warnings(IO, atom_to_list(Command), Warnings);
send_load_warnings(IO, Command, Warnings) ->
  Pred = fun(X) -> case X of flat_pkg -> true;
                             {_Module, flat_pkg} -> true;
                             true -> false end end,
  {Flat, _NotFlat} = lists:partition(Pred, Warnings),
  TotalFlat = length(Flat),
  if length(Flat) > 2   ->
       ?STDERR("~s: flat packages unsafe (~p total)~n", [Command, TotalFlat]),
       send_load_warnings(IO, Command, Warnings, true);
     true               ->
       send_load_warnings(IO, Command, Warnings, false)
  end.

%%
%% Hidden functions
%%

% @hidden Smart argument lookup function for ?ARGV(X) macro.
argv(ARG, N) ->
  if N == 0 -> ARG#arg.cmd; N > 0 -> lists:nth(N, ARG#arg.v) end.

%%
%% Local Functions
%%

% Run pose-compliant command.
% @hidden shouldn't need to be called fully qualified...
-spec spawn(IO :: #std{}, Command :: command(), Param :: [any()],
            Module :: module()) -> pid().
spawn(MyIO, Command, Param, Module) ->
  IO = ?IO(self(), self(), MyIO#std.err),
  ARG = ?ARG(Command, Param),
  CmdPid = spawn_link(Module, run, [IO, ARG, ?ENV]),
  ?DEBUG("Running ~p as ~p ~p~n", [Command, Module, CmdPid]),
  CmdPid.

%%%
% Send load warnings
%%%

% Send warning messages for namespace collisions and some flat packages
send_load_warnings(_IO, _Command, [], _ManyFlat) -> ok;
send_load_warnings(IO, Command, [Head | Tail], ManyFlat) ->
  case Head of
    diff_path                                   ->
      ?STDERR("~s: namespace collision~n", [Command]);
    flat_pkg                                    ->
      if ManyFlat == false  -> ?STDERR("~s: flat package unsafe~n", [Command]);
         true               -> false
      end;
    {Module, diff_path}                         ->
      ?STDERR("~p: namespace collision~n", [Module]);
    {Module, flat_pkg}                          ->
      if ManyFlat == false  -> ?STDERR("~s: flat package unsafe~n", [Module]);
         true               -> false
      end
  end,
  send_load_warnings(IO, Command, Tail, ManyFlat).
