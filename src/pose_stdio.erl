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

%% @doc Standard I/O functions underlying `interface.hrl' macros.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @version 0.2.3
-module(pose_stdio).
-version("0.2.3").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-include_lib("kernel/include/file.hrl").

-define(POSIX, [eacces, eagain, ebadf, ebusy, edquot, eexist, efault,
                efbig, eintr, einval, eio, eisdir, eloop, emfile,
                emlink, enametoolong, enfile, enodev, enoent, enomem, enospc,
                enotblk, enotdir, enotsup, enxio, eperm, epipe, erofs,
                espipe, esrch, estale, exdev]).
-define(FILE_ERR, ?POSIX ++ [badarg, terminated, system_limit]).

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
  IsString = is_string(Output),
  if IsString   -> get_debug() ! {debug, self(), Output};
     true       -> send_debug("~p", [Output])
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
      String = format_erlerr_trace(Atom, Data, Trace);
    {Atom, [Head | Tail]} when is_atom(Atom), is_tuple(Head)    ->
      String = format_erlerr_trace(Atom, [], [Head | Tail]);
    {Atom, Data} when is_atom(Atom)                             ->
      List = format_erlerr_file(Atom),
      String = io_lib:format("~s: ~s", [List, format_erlerr(Data)]);
    Atom when is_atom(Atom)										->
      String = format_erlerr_file(Atom);
    _Else                                                       ->
      String = format_erlerr_else(What)
  end,
  lists:flatten(String).

%%
%% Local Functions
%%

format_erlerr_file(Atom) ->
  IsFileErr = lists:member(Atom, ?FILE_ERR),
  if IsFileErr  -> file:format_error(Atom);
     true       -> format_erlerr_else(Atom)
  end.

format_erlerr_trace(Atom, [], Trace) ->
  Format = "~s ~p~nTrace: ~p~n",
  String = format_erlerr_file(Atom),      
  io_lib:format(Format, [String, self(), Trace]);
format_erlerr_trace(Atom, Reason, Trace) ->
  Format = "~s ~p~nReason: ~p~nTrace: ~p~n",
  String = format_erlerr_file(Atom),
  io_lib:format(Format, [String, self(), Reason, Trace]).

format_erlerr_else({List, Data}) when is_list(List) ->
  IsString = is_string(List),
  if IsString	-> io_lib:format("~s: ~s", [List, format_erlerr(Data)]);
     true		-> io_lib:format("~p", [{List, Data}])
  end;
format_erlerr_else(Atom) when is_atom(Atom) ->
  String = atom_to_list(Atom), re:replace(String, "_", " ");
format_erlerr_else(What) ->
  IsString = is_string(What),
  if IsString 	-> io_lib:format("~s", [What]);
     true		-> io_lib:format("~p", [What])
  end.

% Send output as #std IO message.
send(_IO, Output, OutPid, Stdout, Erlout) ->
  IsString = is_string(Output),
  if IsString;
     Output == eof      -> OutPid ! {Stdout, self(), Output};
     is_tuple(Output);
     is_atom(Output)    -> OutPid ! {Erlout, self(), Output};
     true               -> String = safe_format("~p", [Output]),
                           OutPid ! {Stdout, self(), String}
  end, ok.

send(IO, Format, What, OutPid, Stdout, Erlout) ->
  send(IO, safe_format(Format, What), OutPid, Stdout, Erlout).

% Throw an exception if we fail to initialize debugger.
get_debug() ->
  case get(debug) of
    Pid when is_pid(Pid)    -> Pid;
    _Else                   -> throw({debug_uninitialized, self()})
  end.

% Don't let bad arguments supress error reporting.
safe_format(Format, What) ->
  try io_lib:format(Format, What)
  catch
    error:badarg ->
      io_lib:format("format: badarg: ~s, ~p~nTrace: ~p~n",
                    [Format, What, erlang:get_stacktrace()])
  end.

%% Erlang's printable_list functions don't acknowledge escape and control
%% characters as legitimate, but shell sometimes passes these to us, so
%% we need a more lenient method for detecting strings.
is_string(Data) ->
  if is_list(Data)  -> is_legit_string(lists:flatten(Data));
     true           -> false
  end.

is_legit_string([First | Rest]) when is_integer(First), First > -1 ->
  Printable = io_lib:printable_unicode_list([First | Rest]),
  if Printable	-> true;
    true		-> is_legit_string(Rest)
  end;
is_legit_string(_List) -> false.