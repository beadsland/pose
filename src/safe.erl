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

-module(safe).

-version("0.0.1").

%%
%% Include files
%%

%-define(debug, true).
-include("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API
-export([format/2]).

% Hidden
-export([format_run/2, format_loop/3]).

%%
%% API Functions
%%

%% @doc Return a formatted string, catching any badarg runtime errors.
-type format() :: io:format().
-spec format(Format :: format(), What :: [any()]) -> string().
%
format(Format, What) ->
  SafePid = spawn_link(?MODULE, safe_run, [Format, What]),
  ?MODULE:format_loop(SafePid, Format, What).

%%
%% Hidden Functions
%%

% @hidden Fully qualified loop.
format_loop(SafePid, Format, What) ->
  receive
    {purging, _Pid, _Mod}             ->
      ?MODULE:format_loop(SafePid, Format, What);
    {'EXIT', SafePid, {ok, String}}   ->
      {ok, String};
    {'EXIT', SafePid, Reason} ->
      List = [Format, What, ?FORMAT_ERLERR(Reason)],
      {error, io_lib:format("Format: ~s~nList: ~p~nError~p~n", List)};
    Noise                             ->
      ?DEBUG("noise: ~p ~p~n", [Noise, self()]),
      ?MODULE:format_loop(SafePid, Format, What)
  end.

% @hidden Spawned as a separate process, to catch runtime errors.
format_run(Format, What) ->
  String = io_lib:format(Format, What),
  exit({ok, String}).

%%
%% Local Functions
%%