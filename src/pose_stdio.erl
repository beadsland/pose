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

%% @version 0.2.5
-module(pose_stdio).
-version("0.2.5").

%%
%% Include files
%%

-define(debug, true).
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
format_erlerr({Term, [Head | Tail]}) when is_tuple(Head) ->
  Reason = format_erlerr(Term),
  Trace = format_erltrace([Head | Tail]),
  StripTrace = string:strip(lists:flatten(Trace), right, $\n),
  io_lib:format("~s ~p~n~s", [Reason, self(), StripTrace]);
format_erlerr({Term, Data}) ->
  String1 = format_erlerr(Term), 
  String2 = lists:flatten(format_erlerr(Data)),
  [Line1 | _Rest] = string:tokens(String2, "\n"),
  Length = string:len(String1) + string:len(Line1),
  if Length > 75    -> io_lib:format("~s:~n     ~s", [String1, String2]);
     true           -> io_lib:format("~s: ~s", [String1, String2])
  end;
format_erlerr(Atom) when is_atom(Atom) ->
  IsFileErr = lists:member(Atom, ?FILE_ERR),
  if IsFileErr  -> file:format_error(Atom);
     true       -> Options = [{return, list}, global],
                   re:replace(atom_to_list(Atom), "_", " ", Options)
  end;
format_erlerr(Else) ->
  IsString = is_string(Else),
  if IsString   -> io_lib:format("~s", [Else]);
     true       -> String = io_lib:format("      ~72p", [Else]),
                   re:replace(String, "^\s*", "", [{return, list}])
  end.

%%
%% Local Functions
%%

% Smartly format erl stack traces.
format_erltrace([]) -> [];
format_erltrace([{Module, Func, Arity, Source} | Tail]) 
                                                     when is_integer(Arity) ->
  StackPop = format_erltrace(Module, Func, Arity, Source),
  io_lib:format("~s~n~s", [StackPop, format_erltrace(Tail)]); 
format_erltrace([{Module, Func, Param, Source} | Tail]) when is_list(Param) ->
  Format = "~s~n~s~n~s",
  StackPop = format_erltrace(Module, Func, length(Param), Source),
  Return = [{return,list}],
  TopFunc = re:replace(StackPop, "in call from", "in function", Return),
  CalledAs = format_erltrace(Module, Func, Param),
  io_lib:format(Format, [TopFunc, CalledAs, format_erltrace(Tail)]);
format_erltrace([Noise | Tail]) -> 
  io_lib:format("~p~s", [Noise, format_erltrace(Tail)]).

% Smartly format a function with parameters from the stack trace.
format_erltrace(Module, Func, Params) ->
  Format = "   called as   ~p:~p~p",
  String = io_lib:format(Format, [Module, Func, Params]),
  Return = [{return,list}],
  re:replace(re:replace(String, "\\[", "(", Return), "\\]$", ")", Return).

% Smartly format a function popped from the stack strace.
format_erltrace(Module, Func, Arity, Source) ->
  Format = "   in call from ~s:~p/~p~s",
  SrcStr = format_erlsrc(Source),
  io_lib:format(Format, [atom_to_list(Module), Func, Arity, SrcStr]).

format_erlsrc([{file, _File}, {line, Line}]) -> 
  io_lib:format(", line ~p", [Line]);
format_erlsrc([]) -> [];
format_erlsrc(Else) -> io_lib:format(", ~p", [Else]).
  
% Send output as #std IO message.
send(_IO, Output, OutPid, Stdout, Erlout) ->
  case Output of
    eof -> ?DEBUG("saw eof\n");
    _   -> false
  end,
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