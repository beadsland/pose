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

%% @version 0.1.6
-module(pose).
-version("0.1.6").

%%
%% Include files
%%

-define(debug, true).
-include("interface.hrl").

%%
%% Exported Functions
%%

% pose entry functions
-export([start/1]).

% hidden functions
-export([loop/2, argv/2]).

%%
%% API Functions
%%

%% Run a pose-compliant command from the erl commandline.
-spec start([Command :: atom()]) -> ok | no_return().
start([Command]) when is_atom(Command) ->
  IO = ?IO(self()),
  ?INIT_POSE,
  ?DEBUG("Starting pose ~p ~p~n", [Command, self()]),
  case pose_code:load(Command) of
    {module, Module, Warning}   ->
      Erlerr = ?FORMAT_ERLERR({pose, {Command, Warning}}),
      io:format(standard_error, "** ~p~n", [Erlerr]),
      spawn_run(IO, Command, Module);
    {module, Module}            ->
      spawn_run(IO, Command, Module);
    {error, What}               ->
      Erlerr = ?FORMAT_ERLERR({pose, {Command, What}}),
      io:format(standard_error, "** ~p~n", [Erlerr]),
      exit({Command, What})
  end.

spawn_run(IO, Command, Module) ->
  RunPid = spawn_link(Module, run, [IO, ?ARG(Command), ?ENV]),
  ?DEBUG("Running ~p as ~p ~p~n", [Command, Module, RunPid]),
  ?MODULE:loop(Command, RunPid).

%%
%% Hidden functions
%%

% @hidden Fully qualified loop waiting for output and then exiting.
loop(Command, RunPid) ->
  SelfPid = self(),
  receive
    {purging, _Pid, _Mod}       -> ?MODULE:loop(Command, RunPid);
    {'EXIT', RunPid, ok}        -> ok;
    {'EXIT', RunPid, Reason}    -> exit({Command, Reason});
    {debug, SelfPid, Output}    -> do_output(Command, RunPid, debug, Output);
    {MsgTag, RunPid, Output}    -> do_output(Command, RunPid, MsgTag, Output);
    Noise                       -> do_noise(Command, RunPid, Noise)
  end.

% @hidden Smart argument lookup function for ?ARGV(X) macro.
argv(ARG, N) ->
  if N == 0 -> ARG#arg.cmd; N > 0 -> lists:nth(N, ARG#arg.v) end.

%%
%% Local Functions
%%

%%%
% Loop handlers
%%%

% Relay standard output to console.
do_output(Command, RunPid, MsgTag, Output) ->
  case MsgTag of
    erlout  -> io:format("~p: ~p~n", [Command, Output]);
    erlerr  -> io:format(standard_error, "** ~p: ~s~n",
                         [Command, ?FORMAT_ERLERR(Output)]);
    stdout  -> io:format(Output);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end,
  ?MODULE:loop(Command, RunPid).

% Handle noise on message queue.
do_noise(Command, RunPid, Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]),
  ?MODULE:loop(Command, RunPid).
