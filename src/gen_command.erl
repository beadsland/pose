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

%% @doc Generic `pose' command behaviour.  Provides helper functions for
%% handling command entry points.
%%
%% Each `pose' compatible command module will use the following pattern
%% when implementing this behaviour:
%%
%% <pre>
%% -spec start() -> no_return().
%% %% @equiv start([])
%% start() -> start([]).
%%
%% -spec start(Param :: [atom()]) -> no_return().
%% %% @@doc Start as a blocking function.
%% start(Param) -> gen_command:start(Param, ?MODULE).
%%
%% -spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% %% doc Start as a `pose' command.
%% run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).
%%
%% %% @@private Callback entry point for gen_command behaviour.
%% do_run(IO, ARG) -> <i>command functionality goes here</i>.
%% </pre>
%%
%% The `do_run/2' function should finish with an `exit/1', either with
%% a zero status (`ok' or `{ok, any()}') or a non-zero status (any other
%% value.  Note that the implicit default exit status, `normal' is treated
%% as a non-zero status by `pose'.  Use an explicit `exit(ok)', instead.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-module(gen_command).

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

-export([behaviour_info/1]).

% entry functions
-export([start/2, run/4]).

% helper functions
-export([load_command/2]).

% private functions
-export([loop/2]).

%%
%% API Functions
%%

-type callback() :: {function(), arity()}.
-spec behaviour_info(callbacks) -> [callback()] | undefined.
% Callback list for modules implementing `gen_command' behaviour.
behaviour_info(callbacks) -> [{start, 0}, {start, 1}, {run, 3}, {do_run, 2}];
behaviour_info(_) -> undefined.

-spec start(Param :: [any()], Module :: module()) -> no_return().
% Start as a `pose' command as a blocking function.
start(Param, Module) ->
  IO = ?IO(self()),
  ARG = ?ARG(Module, Param),
  RunPid = spawn_link(?MODULE, run, [IO, ARG, ?ENV, Module]),
  ?MODULE:loop(IO, RunPid).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{},
          Module :: module()) -> no_return().
% Run as a `pose' command, spawned as a linked process.
run(IO, ARG, ENV, Module) ->
  ?INIT_POSE,
  Module:do_run(IO, ARG).

-type load_rtn() :: {module, module()} | {error, pose_code:load_err()}.
-spec load_command(IO :: #std{}, Command :: pose:command()) ->  load_rtn().
% Load a pose command, sending off any warnings returned.
load_command(IO, Command) ->
  case pose_command:load(Command) of
    {module, Module, Warnings}  ->
      pose:send_load_warnings(IO, Command, Warnings),
      {module, Module};
    {error, What, Warnings}     ->
      pose:send_load_warnings(IO, Command, Warnings),
      {error, What}
  end.

%%
%% Local Functions
%%

%%%
% Start loop
%%%

% @private Export to allow for hotswap.
loop(IO, RunPid) ->
  SelfPid = self(),
  receive
    {purging, _Pid, _Mod}           -> ?MODULE:loop(IO, RunPid);
    {'EXIT', RunPid, ok}            -> ok;
    {'EXIT', RunPid, {ok, What}}    -> do_output(erlout, What), {ok, What};
    {'EXIT', RunPid, Reason}        -> do_output(erlerr, Reason), Reason;
    {'EXIT', OtherPid, normal}      -> do_other_exit(OtherPid),
                                       ?MODULE:loop(IO, RunPid);
    {debug, SelfPid, Output}        -> do_output(debug, Output),
                                       ?MODULE:loop(IO, RunPid);
    {MsgTag, RunPid, Output}        -> do_output(MsgTag, Output),
                                       ?MODULE:loop(IO, RunPid);
    Noise                           -> do_noise(Noise),
                                       ?MODULE:loop(IO, RunPid)
  end.

% Handle extraneous exit messages
do_other_exit(OtherPid) ->
  Msg = io_lib:format("Saw ~p exit~n", [OtherPid]),
  do_output(debug, Msg).

% Handle stderr and stdout messages.
do_output(MsgTag, Output) ->
  case MsgTag of
    stdout  -> io:format("~s", [Output]);
    erlout  -> io:format("data: ~p~n", [Output]);
    erlerr  -> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end.

% Handle message queue noise.
do_noise(Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]).