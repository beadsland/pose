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

%% @doc Standard I/O functions underlying `interface.hrl' macros.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.2.0
-module(pose_stdio).
-version("0.2.0").

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").

-include("interface.hrl").

%%
%% Exported Functions
%%

% Standard I/O
-export([send_stderr/2, send_stderr/3, send_stdout/2, send_stdout/3,
         send_debug/2, format_erlerr/1]).

%%
%% API Functions
%%

%%%
% Standard I/O functions
%%%

%% @doc Smart STDOUT/2 macro function.
-type format() :: io:format().
-spec send_stdout(IO :: #std{}, Format :: format(), What :: list()) -> ok.
%
send_stdout(IO, Format, What) ->
  send_stdout(IO, safe:format(Format, What)),
  ok.

%% @doc Smart STDOUT/1 macro function.
-type output() :: {atom(), any()} | string().
-spec send_stdout(IO :: #std{}, What :: output()) -> ok.
%
send_stdout(IO, What) ->
  if is_tuple(What);
     is_atom(What)  -> IO#std.out ! {erlout, self(), What};
     is_list(What)  -> IO#std.out ! {stdout, self(), What};
     true           -> ?STDOUT("~p", [What])
  end,
  ok.

%% @doc Smart STDERR/2 macro function.
-spec send_stderr(IO :: #std{}, Format:: format(), What :: list()) -> ok.
send_stderr(IO, Format, What) ->
  send_stderr(IO, safe:format(Format, What)),
  ok.

%% @doc Smart STDERR/1 macro function.
-spec send_stderr(IO :: #std{}, What :: output()) -> ok.
send_stderr(IO, What) ->
  if is_tuple(What);
     is_atom(What)  -> IO#std.out ! {erlerr, self(), What};
     is_list(What)  -> IO#std.out ! {stderr, self(), What};
     true           -> ?STDERR("~p", [What])
  end,
  ok.

%% @doc Smart DEBUG/2 macro function.
%% Retrieves debug pid from process dictionary.  (Set by macro.)
%% @end
-spec send_debug(Format :: format(), What :: list()) -> ok.
%
send_debug(Format, What) ->
  Msg = safe:format(Format, What),
  case get(debug) of
    Pid when is_pid(Pid) ->
      get(debug) ! {debug, self(), Msg}, ok;
    _Else                ->
      throw({debug_unitialized, Msg})
  end.

%% @doc Smartly format erlerr messages.
-spec format_erlerr(What :: any()) -> string().
format_erlerr(What) ->
    case What of
        {{Atom, Data}, Trace} when is_atom(Atom), is_list(Trace)    ->
            Format = "~p ~p~nReason: ~p~nTrace: ~p~n",
            NewWhat = [Atom, self(), Data, Trace],
            String = io_lib:format(Format, NewWhat);
        {Atom, [Head | Tail]} when is_atom(Atom), is_tuple(Head)    ->
            Format = "~p ~p~nTrace: ~p~n",
            NewWhat = [Atom, self(), [Head | Tail]],
            String = io_lib:format(Format, NewWhat);
        {Atom, Data} when is_atom(Atom)                             ->
            String = io_lib:format("~p: ~s", [Atom, format_erlerr(Data)]);
        List when is_list(List)                                     ->
            String = io_lib:format("~s", [List]);
        _Else                                                       ->
            String = io_lib:format("~p", [What])
    end,
    lists:flatten(String).


%%
%% Local Functions
%%
