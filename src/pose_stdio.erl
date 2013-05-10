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

%% @version 0.2.6
-module(pose_stdio).
-version("0.2.6").

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

-define(RUNTIME1, [badarg, badarith, function_clause, if_clause, noproc, 
                   notalive, system_limit, timeout_value, undef]).

-define(RUNTIME2, [badarg, badarity, badfun, badmatch, case_clause, try_clause, 
                   argument_limit, bad_filter, bad_generator, unbound]).

-define(RUNTIME4, [shell_undef]).

-define(RUNSHELL1, [restricted_shell_started, restricted_shell_stopped]).

-define(RUNSHELL2, [restricted_shell_bad_return, restricted_shell_disallowed]).

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
  format_erldump(Term, [Head | Tail]);
format_erlerr({Term, Data}) -> format_erltwotup(Term, Data);
format_erlerr(Atom) when is_atom(Atom) -> format_erlatom(Atom);
format_erlerr(Else) -> format_erlelse(Else).

%%
%% Local Functions
%%

% Smartly format 2-tuples consisting of an error term followed by a stack trace.
format_erldump(Term, Stack) ->
  Reason = format_erlrun(Term, Stack), 
  Trace = format_erltrace(Stack),
  StripTrace = string:strip(lists:flatten(Trace), right, $\n),
  io_lib:format("~s ~p~n~s", [Reason, self(), StripTrace]).

% Format as Erlang expection if runtime error, otherwise handle locally.
format_erlrun(Atom, [Head | _Tail]) when is_atom(Atom) -> 
  IsRuntime = lists:member(Atom, ?RUNTIME1),
  IsRunshell = lists:member(Atom, ?RUNSHELL1),
  if IsRuntime  -> format_erlrun(Atom, [Head], error);
     IsRunshell -> format_erlrun(Atom, [Head], exit);
     true       -> format_erlerr(Atom)
  end;
format_erlrun({Atom, Term}, [Head | _Tail]) when is_atom(Atom) ->
  IsRuntime = lists:member(Atom, ?RUNTIME2),
  IsRunshell = lists:member(Atom, ?RUNSHELL2),
  if IsRuntime  -> format_erlrun({Atom, Term}, [Head], error);
     IsRunshell -> format_erlrun({Atom, Term}, [Head], exit);
     true       -> format_erlerr({Atom, Term})
  end;
format_erlrun({Atom, T1, T2, T3}, [Head | _Tail]) when is_atom(Atom) ->
  IsRuntime = lists:member(Atom, ?RUNTIME2),
  if IsRuntime  -> format_erlrun({Atom, T1, T2, T3}, [Head], error);
     true       -> format_erlerr({Atom, T1, T2, T3})
  end.

% Try to obtain Erlang/OTP error string.
format_erlrun(Reason, Stack, Class) ->
  PF = fun(Term, _I1) -> Term end, SF = fun(_M, _F, _A) -> false end,
  Try = (catch lib:format_exception(1, Class, Reason, Stack, SF, PF)),
  format_erlrun(Reason, Stack, Class, Try).

% If successfully obtained runtime error string, strip extraneous text.
format_erlrun(_Reason, _Stack, _Class, String) when is_list(String) ->
  [Except | _] = string:tokens(String, "\n"), Except;
format_erlrun(Reason, _Stack, _Class, _) -> format_erlerr(Reason).
  
% Smartly format 2-tuples of arbitrary terms, which is how we expect all deep
% errors to be formed.
format_erltwotup(Term, Data) ->
  String1 = lists:flatten(format_erlerr(Term)), 
  String2 = lists:flatten(format_erlerr(Data)),
  [Line1 | _Rest] = string:tokens(String2, "\n"),
  Length = string:len(String1) + string:len(Line1),
  if Length > 72    -> io_lib:format("~s:~n     ~s", [String1, String2]);
     true           -> io_lib:format("~s: ~s", [String1, String2])
  end.

% Smartly format error atoms, including POSIX and runtime errors.
format_erlatom(Atom) ->
  IsFileErr = lists:member(Atom, ?FILE_ERR),
  if IsFileErr  -> file:format_error(Atom);
     true       -> Options = [{return, list}, global],
                   re:replace(atom_to_list(Atom), "_", " ", Options)
  end.

%PF = fun(_, _) -> [] end, SF = fun(_, _, _) -> [] end,
%                   [_, Err] = lib:format_exception(1, error, Atom, [], SF, PF),
%                   format_erlatom(Atom, Err)
%format_erlatom(Atom, []) ->
%format_erlatom(_Atom, Err) -> Err.
 

% Smartly format strings, nested deep lists, complex tuples, and whatever else.
format_erlelse(Else) ->
  IsString = is_string(Else),
  if IsString   -> io_lib:format("~s", [Else]);
     true       -> String = io_lib:format("      ~72p", [Else]),
                   re:replace(String, "^\s*", "", [{return, list}])
  end.

% Smartly format Erlang stack traces.
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
  CalledAs = format_erlfunc(Module, Func, Param),
  io_lib:format(Format, [TopFunc, CalledAs, format_erltrace(Tail)]);
format_erltrace([Noise | Tail]) -> 
  io_lib:format("~p~s", [Noise, format_erltrace(Tail)]).

% Smartly format a function popped from the stack strace.
format_erltrace(Module, Func, Arity, Source) ->
  Format = "   in call from ~s:~p/~p~s",
  SrcStr = format_erlsrc(Source),
  io_lib:format(Format, [atom_to_list(Module), Func, Arity, SrcStr]).

% Smartly format the source file and line of a function call.
format_erlsrc([{file, _File}, {line, Line}]) -> 
  io_lib:format(", line ~p", [Line]);
format_erlsrc([]) -> [];
format_erlsrc(Else) -> io_lib:format(", ~p", [Else]).

% Smartly format a function with parameters from the stack trace.
format_erlfunc(Module, Func, Params) ->
  Format = "   called as   ~s:~p~p",
  Str = io_lib:format(Format, [atom_to_list(Module), Func, Params]),
  Return = [{return,list}],
  Paren = re:replace(re:replace(Str, "\\[", "(", Return), "\\]$", ")", Return),
  Tokens = string:tokens(lists:flatten(Paren), "\n"),
  format_erlfunc(Module, Func, Params, Tokens).

% If first line is too long, move its parameters to second line.
format_erlfunc(_Module, Func, Params, [Head | Tail]) ->
  HeadLen = string:len(Head),
  if HeadLen > 80 -> [Func | Params] = strings:tokens(Head, "("),
                     Line1 = Func, 
                     Indent = string:copies("\s", string:len(Line1)),
                     Line2 = string:concat(Indent, strings:join(Params, $()),
                     Lines = [Line1 | [Line2 | Tail]];
     true         -> Lines = [Head | Tail]
  end,
  undent_erlfunc(Lines).

% Reduce hanging indents in event of overlong lines.
undent_erlfunc([First | Rest]) -> undent_erlfunc(First, Rest).

undent_erlfunc(First, Rest) ->
  Over = lists:max([string:len(X) || X <- Rest]) - 80,
  Limit = lists:min([Over, string:len(First)]),
  undent_erlfunc(First, Rest, Limit).

undent_erlfunc(First, Rest, Over) when Over =< 0 -> 
  string:join([First | Rest], "\n");
undent_erlfunc(First, Rest, Over) ->
  undent_erlfunc(First, [lists:delete($\s, X) || X <- Rest], Over - 1).

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
      io_lib:format("format: badarg: ~s, ~p~n~s",
                    [Format, What, format_erltrace(erlang:get_stacktrace())])
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