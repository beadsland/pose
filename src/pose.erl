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

%% @version 0.1.5
-module(pose).
-version("0.1.5").

%%
%% Include files
%%

-define(debug, true).
-include("interface.hrl").

%%
%% Exported Functions
%%

% used from erl command line
-export([start/1]).

% exposed for use by start/1
-export([start_loop/2]).

%%
%% API Functions
%%

%% Locate command on PATH, load and run.
-spec start([Command :: atom()]) -> ok | no_return().
start([Command]) ->
  IO = ?IO(self()),
  ?INIT_POSE,
  ?DEBUG("pose: starting ~p~n", [Command]),
  case start_code:load(Command) of
    {module, Module, Warning}   -> io:format("pose: warn: ~p~n", [Warning]),
                                   do_start(IO, Module);
    {module, Module}            -> do_start(IO, Module);
    {error, What}               -> Error = ?FORMAT_ERLERR(What),
                                   io:format("~p: ~s~n", [Command, Error]),
                                   exit({Command, What})
  end.

%%
%% Local Functions
%%

%%%
% Start from commandline
%%%

% Run module as pose process and exit on result.
do_start(IO, Module) ->
  RunPid = spawn_link(Module, run, [IO]),
  ?MODULE:start_loop(Module, RunPid).

% Loop waiting for output and exit.
start_loop(Module, RunPid) ->
  SelfPid = self(),
  receive
    {purging, _Pid, _Mod}       -> ?MODULE:start_loop(Module, RunPid);
    {'EXIT', RunPid, ok}        -> ok;
    {'EXIT', RunPid, Reason}    -> exit({Module, Reason});
    {debug, SelfPid, Output}    -> start_output(Module, RunPid,
                                                debug, Output);
    {MsgTag, RunPid, Output}    -> start_output(Module, RunPid,
                                                MsgTag, Output);
    Noise                       -> start_noise(Module, RunPid, Noise)
  end.

% Relay standard output to console.
start_output(Module, RunPid, MsgTag, Output) ->
  case MsgTag of
    erlout  -> io:format("~p: ~p~n", [Module, Output]);
    erlerr  -> io:format(standard_error, "** ~p: ~s~n",
                         [Module, ?FORMAT_ERLERR(Output)]);
    stdout  -> io:format(Output);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end,
  ?MODULE:start_loop(Module, RunPid).

% Handle noise on message queue.
start_noise(Module, RunPid, Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]),
  ?MODULE:start_loop(Module, RunPid).
