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
%% @copyright 2012, 2013 Beads D. Land-Trujillo

-module(gen_command).

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

-export([behaviour_info/1]).

% entry functions
-export([start/2, run/4]).

% helper functions
-export([load_command/2]).

% macro functions
-export([get_version/1]).

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
  IO = ?IO(self(), true), % assume erlang io with no ctrl-d available for eof
  pose:init(IO, pose:startenv()),
  ARG = ?ARG(Module, Param),
  RunPid = spawn_link(?MODULE, run, [IO, ARG, ?ENV, Module]),
  ?MODULE:loop(IO, RunPid).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{},
          Module :: module()) -> no_return().
% Run as a `pose' command, spawned as a linked process.
run(IO, ARG, ENV, Module) ->
  pose:init(IO, ENV),
  put(command, Module),
  Module:do_run(IO, ARG).

-type load_return() :: {module, module()} | {error, pose_code:load_err()}.
-spec load_command(IO :: #std{}, Command :: pose:command()) ->  load_return().
% Load a pose command, sending off any warnings returned.
load_command(IO, Command) ->
  case pose_command:load(Command) of
    {module, Module, Warnings}  ->
      pose:send_load_warnings(IO, Command, Warnings),
      {module, Module};
    {error, Reason, Warnings}     ->
      pose:send_load_warnings(IO, Command, Warnings),
      {error, Reason}
  end.

-spec get_version(Module :: module()) -> string().
%% @doc Smart VERSION/1 macro function.
%% @deprecated
get_version(Module) ->
  Suffix = case init:get_argument(deps) of
    {ok, [["deps"]]} 	-> "";
    {ok, [[""]]} 		-> ""; 
    {ok, [[Value]]} 	-> "-" ++ Value;
    _					-> "" 
  end,
  ?ATTRIB(Module, version) ++ Suffix.

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
    {'EXIT', RunPid, Reason}        -> do_erlexit(RunPid, Reason);
    {'EXIT', ExitPid, normal}       -> ?DOEXIT, ?MODULE:loop(IO, RunPid);
    {debug, SelfPid, Output}        -> do_output(debug, Output),
                                       ?MODULE:loop(IO, RunPid);
    {stdin, RunPid, captln}			-> do_input(RunPid),
                                       ?MODULE:loop(IO, RunPid);
    {MsgTag, RunPid, Output}        -> do_output(MsgTag, Output),
                                       ?MODULE:loop(IO, RunPid);
    Noise                           -> ?DONOISE, ?MODULE:loop(IO, RunPid)
  end.

% Insinuate RunPid into formatted erlerr output string.
do_erlexit(RunPid, Reason) ->
  [First | Rest] = string:tokens(lists:flatten(?FORMAT_ERLERR(Reason)), "\n"),
  Reperr = re:replace(First, ":$", ":\s", [{return, list}]),
  case string:str(Reperr, ": ") of
    0       -> NewFirst = io_lib:format("~s ~p", [Reperr, RunPid]);
    Start   -> PreString = string:left(Reperr, Start-1),
               String1 = io_lib:format("~s ~p", [PreString, RunPid]),
               String2 = string:substr(Reperr, Start+2),
               case String2 of
                 [] -> NewFirst = io_lib:format("~s:", [String1]);
                 _  -> NewFirst = ?FORMAT_ERLERR({String1, String2})
               end
  end,
  NewReason = string:join([NewFirst | Rest], "\n"),
  do_output(erlerr, NewReason),
  NewReason.

% Handle captured line from standard input
do_input(RunPid) ->
  case io:get_line("") of
    {error, Reason}	-> Erlerr = ?FORMAT_ERLERR(Reason),
                       io:format(standard_error, "** stdin: ~s", [Erlerr]);
    Line			-> RunPid ! {stdout, self(), Line}
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