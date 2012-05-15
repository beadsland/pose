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
-export([start/1, spawn/2, spawn/3]).
-export_type([command/0]).

% hidden functions
-export([loop/2, argv/2, spawn/4]).

%%
%% API Functions
%%

%% Run a pose-compliant command from the erl commandline.
-spec start([Command :: atom()]) -> ok | no_return().
start([Command]) ->
  IO = ?IO(self()),
  ?INIT_POSE,
  io:format("Starting pose ~p~n", [self()]),
  case ?MODULE:spawn(IO, Command) of
    {error, Reason} ->
      Erlerr = ?FORMAT_ERLERR({pose, {Command, Reason}}),
      io:format(standard_error, "** ~s~n", [Erlerr]),
      exit({Command, Reason});
    CmdPid          ->
      ?MODULE:loop(Command, CmdPid)
  end.

%% Run a pose-compliant command from within an application.
%% @equiv spawn(IO, Command, [])
-type command() :: nonempty_string() | atom().
-type spawn_rtn() :: {error, pose_code:load_err()} | pid().
-spec spawn(IO :: #std{}, Command :: command()) -> spawn_rtn().
%
spawn(IO, Command) -> ?MODULE:spawn(IO, Command, []).

%% Run a pose-compliant command from within an application.
-spec spawn(IO :: #std{}, Command :: command(), Param :: [any()]) ->
        spawn_rtn().
%
spawn(IO, Command, Param) when is_atom(Command) ->
  ?MODULE:spawn(IO, atom_to_list(Command), Param);
spawn(IO, Command, Param) ->
  case pose_code:load(Command) of
    {module, Module, diff_path} ->
        ?STDERR("~s: namespace collision~n", [Command]),
        ?MODULE:spawn(IO, Command, Param, Module);
    {module, Module, flat_pkg}  ->
        ?STDERR("~s: flat package unsafe~n", [Command]),
        ?MODULE:spawn(IO, Command, Param, Module);
    {module, Module}            ->
        ?MODULE:spawn(IO, Command, Param, Module);
    {error, Else}               ->
        {error, Else}
  end.

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

% Run pose-compliant command.
% @hidden Only exported to avoid warnings about overriding BIF.
spawn(MyIO, Command, Param, Module) ->
  IO = ?IO(self(), self(), MyIO#std.err),
  ARG = ?ARG(Command, Param),
  CmdPid = spawn_link(Module, run, [IO, ARG, ?ENV]),
  ?DEBUG("Running ~p as ~p ~p~n", [Command, Module, CmdPid]),
  CmdPid.

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
