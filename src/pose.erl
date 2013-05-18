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
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Entry points for running `pose'-compatible commands.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.8
-module(pose).
-version("0.1.8").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Private callbacks
-export([do_run/2]).

% Internal entry functions
-export([exec/2]).

% Behind the curtain initialization
-export([init/2]).

% Process variable functions
-export([setenv/2, env/1, deps/0, path/0]).

% pose_command helper function
-export([send_load_warnings/3]).

% private functions
-export([argv/2]).

%%
%% gen_command API functions
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
%% gen_command callback functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, PoseARG) ->
  ?DEBUG("Starting pose ~p~n", [self()]),
  [Command | Param] = PoseARG#arg.v,
  ARG = ?ARG(Command, Param),
  case gen_command:load_command(IO, Command) of
    {module, Module}    -> Module:run(IO, ARG, ?ENV);
    {error, What}       -> exit({Command, What})
  end.

%%
%% other API functions
%%

-spec exec(IO :: #std{}, ARG :: #arg{}) -> no_return().
%% @doc Execute a command within the current process.
exec(IO, ARG) ->
  init(IO, ?ENV),
  Command = ?ARGV(0),
  ?DEBUG("Executing ~p ~p~n", [Command, self()]),
  case gen_command:load_command(IO, Command) of
    {module, Module}  -> Module:do_run(IO, ARG);
    {error, What}     -> exit(What)
  end.

-spec init(IO :: #std{}, ENV :: #env{}) -> ok.
%% @doc Initialize the search path for `pose' command modules.
init(IO, ENV) ->
  process_flag(trap_exit, true),
  put(debug, IO#std.err),
  put(env, ENV#env.plist),
  setenv('IWD', filename:absname("")),  % no good, IWD needs to be inherited
  DepsPath = filelib:wildcard(filename:join([iwd(), deps(), "*/ebin"])),                               
  setenv('PATH', [filename:join(iwd(), "ebin") | DepsPath]),
  ok.

-spec deps() -> string().
%% @doc Return project subdirectory in which project dependencies are found.
deps() ->
  case init:get_argument(deps) of {ok, [[Value]]} -> Value; true -> "deps" end.

-spec env(Key :: atom()) -> term().
%% @doc Return a value among the `pose' process environment variables.
env(Key) -> proplists:get_value(Key, get(env)).

-spec setenv(Key :: atom(), Value :: term()) -> term().
%% @doc Assign a value to a `pose' process environment variable, such that
%% it will be shared with `pose' subprocesses that inherit the environment.
%% @end
setenv(Key, Value) -> 
  put(env, [{Key, Value} | proplists:delete(Key, get(env))]), Value.

-spec iwd() -> string().
%% @doc Return the initial working directory of the Erlang runtime.
iwd() -> env('IWD').

-spec path() -> list().
%% @doc Return the current search path for `pose' command modules.
path() -> env('PATH').

-type command() :: atom() | string().
-type warning() :: pose_command:load_mod_warn().
-spec send_load_warnings(IO :: #std{}, Command :: command(),
                         Warnings :: [warning()]) -> ok.
%% @doc Send messages to `stderr' process detailing any warnings received from 
%% `pose_command:load/1'.  Flat package errors are consolidated if more than 
%% one, or dropped, if Erlang/OTP release does not support packages.
%% @end
send_load_warnings(_IO, _Command, []) -> ok;
send_load_warnings(IO, Command, Warnings) when is_atom(Command) ->
  send_load_warnings(IO, atom_to_list(Command), Warnings);
send_load_warnings(IO, Command, Warnings) ->
  Release = erlang:system_info(otp_release),
  R15 = if Release < "R16" -> true; true -> false end,
  Pred = fun(X) -> case X of flat_pkg              -> true;
                             {_Module, flat_pkg}   -> true;
                             _Else                 -> false end end,
  {Flat, _NotFlat} = lists:partition(Pred, Warnings),
  TotalFlat = length(Flat),
  if length(Flat) > 2 and R15   ->
       ?STDERR("~s: flat packages unsafe (~p total)~n", [Command, TotalFlat]),
       send_load_warnings(IO, Command, Warnings, true, R15);
     length(Flat) > 2           ->
       ?DEBUG("~s: flat packages ignored (~p total)~n", [Command, TotalFlat]),
       send_load_warnings(IO, Command, Warnings, true, R15);
     true                       ->
       send_load_warnings(IO, Command, Warnings, false, R15)
  end.

%%
%% Private API functions
%%

% @private Smart argument lookup function for ?ARGV(X) macro.
argv(ARG, N) ->
  if N == 0                             -> ARG#arg.cmd;
     N > 0, length(ARG#arg.v) >= N      -> lists:nth(N, ARG#arg.v);
     true                               -> undefined
  end.

%%
%% Local Functions
%%

%%%
% Send load warnings
%%%

% Send warning messages for namespace collisions and some flat packages
send_load_warnings(_IO, _Command, [], _ManyFlat, _R15) -> ok;
send_load_warnings(IO, Command, [Head | Tail], ManyFlat, R15) 
                                                        when is_atom(Head) ->
  send_load_warnings(IO, Command, [{Command, Head} | Tail], ManyFlat, R15);
send_load_warnings(IO, Command, [{Module, Warn} | Tail], ManyFlat, R15) 
                                                        when is_atom(Module) ->
  String = atom_to_list(Module),
  send_load_warnings(IO, Command, [{String, Warn} | Tail], ManyFlat, R15);
send_load_warnings(IO, Command, [{Module, diff_path} | Tail], ManyFlat, R15) ->
  ?STDERR("~s: namespace collision~n", [Module]),
  send_load_warnings(IO, Command, Tail, ManyFlat, R15);
send_load_warnings(IO, Command, [{Module, flat_pkg} | Tail], false, true) -> 
  ?STDERR("~s: flat package unsafe~n", [Module]),
  send_load_warnings(IO, Command, Tail, false, true);
send_load_warnings(IO, Command, [{Module, flat_pkg} | Tail], ManyFlat, R15) ->
  ?DEBUG("~s: flat package ignored~n", [Module]),
  send_load_warnings(IO, Command, Tail, ManyFlat, R15).