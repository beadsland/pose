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

%% @doc Generic `pose' command behaviour.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-module(gen_command).

%%
%% Include files
%%

-include("pose/include/interface.hrl").

%%
%% Exported Functions
%%

-export([behaviour_info/1]).

% entry functions
-export([start/2, run/4]).

% hidden functions
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

%%
%% Local Functions
%%

%%%
% Start loop
%%%

% @hidden Export to allow for hotswap.
loop(IO, RunPid) ->
  SelfPid = self(),
  receive
    {purging, _Pid, _Mod}           -> ?MODULE:loop(IO, RunPid);
    {'EXIT', RunPid, ok}            -> ok;
    {'EXIT', RunPid, {ok, What}}    -> do_output(erlout, What), {ok, What};
    {'EXIT', RunPid, Reason}        -> do_output(erlerr, Reason), Reason;
    {debug, SelfPid, Output}        -> do_output(debug, Output),    
                                       ?MODULE:loop(IO, RunPid);
    {MsgTag, RunPid, Output}        -> do_output(MsgTag, Output),
                                       ?MODULE:loop(IO, RunPid);
    Noise                           -> do_noise(Noise),
                                       ?MODULE:loop(IO, RunPid)
  end.

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