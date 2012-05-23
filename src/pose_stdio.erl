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
-export([send_stderr/2, send_stderr/3,
         send_stdout/2, send_stdout/3,
         send_debug/1, send_debug/2,
         format_erlerr/1]).

%%
%% API Functions
%%

%%%
% Standard Output
%%%

-type format() :: io:format().
-type output() :: {atom(), any()} | atom() | string().

-spec send_stdout(IO :: #std{}, Format :: format(), What :: list()) -> ok.
%% @doc Smart STDOUT/2 macro function.
send_stdout(IO, Format, What) ->
  send(IO, Format, What, IO#std.out, stdout, erlout).

-spec send_stdout(IO :: #std{}, Output :: output()) -> ok.
%% @doc Smart STDOUT/1 macro function.
send_stdout(IO, Output) -> send(IO, Output, IO#std.out, stdout, erlout).

%%%
% Standard Error
%%%

-spec send_stderr(IO :: #std{}, Format :: format(), What :: list()) -> ok.
%% @doc Smart STDERR/2 macro function.
send_stderr(IO, Format, What) ->
  send(IO, Format, What, IO#std.err, stderr, erlerr).

-spec send_stderr(IO :: #std{}, Output :: output()) -> ok.
%% @doc Smart STDERR/1 macro function.
send_stderr(IO, Output) -> send(IO, Output, IO#std.err, stderr, erlerr).

%%%
% Debug
%%%

-spec send_debug(Output :: any()) -> ok | no_return().
%% @doc Smart DEBUG/1 macro function.
send_debug(Output) ->
  if is_list(Output)    -> get_debug() ! {debug, self(), Output};
     true               -> send_debug("~p", [Output])
  end, ok.

-spec send_debug(Format :: format(), What :: list()) -> ok | no_return().
%% @doc Smart DEBUG/2 macro function.
send_debug(Format, What) ->
  get_debug() ! {debug, self(), safe_format(Format, What)}, ok.

%%%
% Format erlerr
%%%

-spec format_erlerr(What :: any()) -> string().
%% @doc Smartly format erlerr messages.
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
        Else                                                        ->
          IsString = io_lib:printable_list(Else),
          if IsString   ->
               String = io_lib:format("~s", [Else]);
             true       ->
               String = io_lib:format("~p", [Else])
          end
    end,
    lists:flatten(String).

%%
%% Local Functions
%%

send(_IO, Output, OutPid, Stdout, Erlout) ->
  IsString = io_lib:printable_list(Output),
  if IsString           -> OutPid ! {Stdout, self(), Output};
     is_tuple(Output);
     is_atom(Output)    -> OutPid ! {Erlout, self(), Output};
     true               -> String = safe_format("~p", [Output]),
                           OutPid ! {Stdout, self(), String}
  end, ok.

send(IO, Format, What, OutPid, Stdout, Erlout) ->
  send(IO, safe_format(Format, What), OutPid, Stdout, Erlout).

get_debug() ->
  case get(debug) of
    Pid when is_pid(Pid)    -> Pid;
    _Else                   -> throw({debug_uninitialized, self()})
  end.

safe_format(Format, What) ->
  try io_lib:format(Format, What)
  catch
    error:badarg ->
      io_lib:format("format: badarg: ~s, ~p~n", [Format, What])
  end.
