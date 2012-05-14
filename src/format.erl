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

%% @doc Safely apply io_lib:format/2, catching badarg runtime errors.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.0.1

-define(module, format).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.0.1").

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

-import(io_lib).

%%
%% Exported Functions
%%

% API direct
-export([run/2]).

% API pose
-export([run/3]).

% Hidden
-export([run_safe/2, loop/3]).

%%
%% API Functions
%%

%% @doc Return a formatted string, catching any badarg runtime errors.
-type format() :: io:format().
-spec run(Format :: format(), What :: [any()]) -> string().
%
run(Format, What) ->
  SafePid = spawn_link(?MODULE, safe_run, [Format, What]),
  ?MODULE:loop(SafePid, Format, What).

%% @doc Print a formatted string, catching any badarg runtime errors.
-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
run(IO, ARG, _ENV) ->
  ?INIT_POSE,
  Format = ARG(1), What = ARG(2),
  SafePid = spawn_link(?MODULE, run_safe, [Format, What]),
  Result = ?MODULE:loop(SafePid, Format, What),
  case Result of
    {ok, String}    -> ?STDOUT(String);
    {error, String} -> ?STDERR(String)
  end,
  exit(Result).

%%
%% Hidden Functions
%%

% @hidden Fully qualified loop.
loop(SafePid, Format, What) ->
  receive
    {purging, _Pid, _Mod}             ->
      ?MODULE:loop(SafePid, Format, What);
    {'EXIT', SafePid, {ok, String}}   ->
      {ok, String};
    {'EXIT', SafePid, {badarg, What}} ->
      {error, io_lib:format("badarg: ~p", [{Format, What}])};
    {'EXIT', SafePid, {Error, What}} ->
      {error, io_lib:format("error: ~p: ~p", [Error, {Format, What}])};
    Noise                             ->
      ?DEBUG("noise: ~p ~p~n", [Noise, self()]),
      ?MODULE:loop(SafePid, Format, What)
  end.

% @hidden Spawned as a separate process, to catch runtime errors.
run_safe(Format, What) ->
  String = io_lib:format(Format, What),
  exit({ok, String}).

%%
%% Local Functions
%%