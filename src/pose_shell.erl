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
%% Copyright 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc Execute multiple commands under an operating system shell.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.9
-module(pose_shell).
-version("0.0.9").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

-define(EXIT_STATUS, "__exit_status__").
-define(TIMEOUT, timer:minutes(1)).

%%
%% Exported Functions
%%

% API entry points

-export([spawn/0, command/2, exit/1]).

% Private exports

-export([run/1, loop/3]).

%%
%% API Functions
%%

-type shell_pid() :: pid().
-spec spawn() -> shell_pid().
%% @doc Spawn an operating system shell interface as a new process.  Process
%% will exit with an error on the first command that returns a non-zero exit
%% status, or else exit with an `ok' upon a successful call to 
%% `pose_shell:exit/1'.
%% @end
spawn() -> spawn_link(?MODULE, run, [?IO(undef, self(), self())]). 

-type command() :: string().
-spec command(ShellPid :: shell_pid(), Command :: command()) -> ok.
%% @doc Send a command to an existing shell process.  Output and error output
%% are returned as `stdout' and `stderr' messages, respectively.  Any non-zero
%% exit status will cause shell interface process to exit with an error.
%% @end
command(ShellPid, Command) -> ShellPid ! {command, self(), Command}, ok.

-spec exit(ShellPid :: shell_pid()) -> ok.
%% @doc Exit an operating system shell process, upon successful completion of
%% all previously submitted commands.
%% @end
exit(ShellPid) -> ShellPid ! {command, self(), exit}, ok.

%%
%% Local Functions
%%

% @private Internal callback.
run(IO) ->
  ENV = ?ENV, ?INIT_POSE,
  {OS, _} = os:type(), run(IO, OS).

% Configure operating-system specific shell.
run(IO, win32) ->
  cygwin_nodosfilewarning(),
  run(IO, win32, pose_file:trim(os:cmd("echo %ComSpec%")));
run(IO, unix) -> run(IO, unix, "/bin/sh").

% Spawn a shell executable, and start loop listening to same.
run(IO, OS, Shell) ->
  Options = [exit_status, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  case do_run(IO, OS, Port) of
    {error, Reason} -> cleanup(IO, Port), 
                       erlang:exit({shell, Reason});
    ok              -> erlang:exit(ok)
  end.

% Tell shell to exit, sending a delayed close message as last resort.
cleanup(IO, Port) ->
  send_command(Port, "exit"),
  case timer:send_after(?TIMEOUT, Port, {self(), close}) of
    {error, Reason} -> ?STDERR("close timer: ~s", ?FORMAT_ERLERR(Reason));
    {ok, _TRef}     -> ok
  end.
  
do_run(IO, unix, Port) -> loop(IO, Port, []);
do_run(IO, win32, Port) ->
  Port ! {self(), {command, "echo off\r\n"}},
  loop(?IO(echo_off, IO#std.out, IO#std.err), Port, []).

% @private exported for fully-qualified calls.
loop(IO, Port, Ignore) ->
  Caller = IO#std.out, Monitor = IO#std.in,
  receive
    {purging, _Pid, _Module}    -> ?MODULE:loop(IO, Port, Ignore);
    {'EXIT', ExitPid, Reason}   -> do_exit(IO, Port, Ignore, ExitPid, Reason);
    {Port, Message}             -> do_stderr(IO, Port, Ignore, Message);
    {erlout, Pid, eof}          -> Erlout = {erlout_eof, 'should not happen'},
                                   {error, {Pid, {erlout, Erlout}}};
    {stdout, Monitor, Line}     -> do_stdout(IO, Port, Ignore, Line);
    {command, Caller, Command} when Monitor == undef
                                -> do_command(IO, Port, Ignore, Command);
    Noise when Monitor == undef -> ?DONOISE, ?MODULE:loop(IO, Port, Ignore)
  after ?TIMEOUT ->
    if Monitor == undef     -> ?MODULE:loop(IO, Port, Ignore);
       Ignore /= []         -> [IgLine | _IgMore] = Ignore,
                               Command = strip_eol(IgLine),
                               {error, {timeout, Command}};
       true                 -> {error, {timeout, Monitor}}
    end
  end.

% Handle exit messages.
do_exit(IO, _Port, _Ignore, ExitPid, Reason) when ExitPid == IO#std.out ->
  {error, {caller, Reason}}; 
do_exit(_IO, Port, _Ignore, ExitPid, Reason) when ExitPid == Port ->
  {error, {port, Reason}};
do_exit(IO, Port, Ignore, ExitPid, ok) when ExitPid == IO#std.in ->
  ?MODULE:loop(?IO(undef, IO#std.out, IO#std.err), Port, Ignore);
do_exit(IO, _Port, _Ignore, ExitPid, Reason) when ExitPid == IO#std.in ->
  {error, {monitor, Reason}};
do_exit(IO, Port, Ignore, ExitPid, normal) ->
  ?DOEXIT, ?MODULE:loop(IO, Port, Ignore);
do_exit(_IO, _Port, _Ignore, ExitPid, Reason) -> {error, {ExitPid, Reason}}.

% Handle stderr messages.
do_stderr(_IO, _Port, _Ignore, {exit_status, 0}) -> ok;
do_stderr(_IO, _Port, _Ignore, {exit_status, N}) -> {error, {exit_status, N}};
do_stderr(IO, Port, Ignore, {data, "echo off\r\n"}) when IO#std.in==echo_off ->
  NewIO = ?IO(undef, IO#std.out, IO#std.err),
  ?MODULE:loop(NewIO, Port, Ignore);
do_stderr(IO, Port, Ignore, {data, _Line}) when IO#std.in == echo_off ->
  %?DEBUG("~s", [Line]),
  ?MODULE:loop(IO, Port, Ignore);
do_stderr(IO, Port, Ignore, {data, Line}) ->
  [IgLine | IgMore] = case Ignore of [] -> ["",[]]; _ -> Ignore end,
  case Line of
    IgLine      -> ?MODULE:loop(IO, Port, IgMore);
    [$" | _]    -> Unquote = string:strip(strip_eol(Line), both, $"),
                   Tokens = string:tokens(Unquote, ":"),
                   do_stderr(IO, Port, Ignore, Line, Tokens);
    _           -> do_stderr(IO, Port, Ignore, Line, [])
  end.

% Handle non-zero exit status when returned by any command.
do_stderr(_IO, _Port, _Ignore, _Line, [?EXIT_STATUS, Code, Command]) ->
  {error, {Command, {exit_status, list_to_integer(Code)}}};
do_stderr(IO, Port, Ignore, Line, _Tokens) ->
  ?STDERR(unix_eol(Line)), ?MODULE:loop(IO, Port, Ignore).

% Handle stdout messages.
do_stdout(IO, Port, Ignore, eof) -> ?MODULE:loop(IO, Port, Ignore);
do_stdout(IO, Port, Ignore, Line) -> 
  ?STDOUT(Line), ?MODULE:loop(IO, Port, Ignore).

% Strip trailing EOL, whether DOS or UNIX.
strip_eol(Line) -> string:strip(string:strip(Line, right, $\n), right, $\r).

% Convert any DOS EOLs to Unix EOLs.
unix_eol(Line) -> {OS, _} = os:type(), unix_eol(Line, OS).

unix_eol(Line, unix) -> Line;
unix_eol([], win32) -> [];
unix_eol([First | [Second | Rest]], win32) when First == $\r, Second == $\n -> 
  [$\n | unix_eol(Rest, win32)];
unix_eol([First | Rest], win32) -> [First | unix_eol(Rest, win32)].

% Get a temp file to receive command's standard output channel.
do_command(IO, Port, _Ignore, exit) ->
  Ig = send_command(Port, "exit"),
  Ignore = case os:type() of {unix, _} -> []; {win32, _} -> [Ig] end,  
  ?MODULE:loop(?IO(exit, IO#std.out, IO#std.err), Port, Ignore);
do_command(IO, Port, Ignore, Command) ->
  case pose_os:get_temp_file() of
    {error, Reason} -> {error, {temp_file, Reason}};
    {ok, Temp}      -> Strip = string:strip(Command, right, $\n),
                       do_command(IO, Port, Ignore, Strip, Temp)
  end.

% Send command to external shell process, wrapping it with a lock file.
do_command(IO, Port, _Ignore, Command, Temp) ->
  Monitor = pose_shout:monitor(Temp),
  Lock = lock(Port, Temp),
  Cmd = send_command(Port, io_lib:format("~s > \"~s\"", [Command, Temp])),
  Status = exit_status(Port, Command),
  Unlock = unlock(Port, Temp),
  case os:type() of
    {unix, _}  -> Ignore = [];
    {win32, _} -> Ignore = lists:append([[Lock, Cmd], Status, [Unlock]])
  end,
  loop(?IO(Monitor, IO#std.out, IO#std.err), Port, Ignore).

% Test the exit status of the last command, returning it if nonzero.
exit_status(Port, Command) -> 
  {OS, _} = os:type(), exit_status(Port, Command, OS).

% Status echoed as "?EXIT_STATUS:n:Command" (double quotes preserved)
exit_status(Port, Command, unix) ->
  Test = ["OUT=$?; test $OUT -ne 0 && echo \"\\\"", 
          ?EXIT_STATUS, ":$OUT:", Command, "\\\"\""],
  [send_command(Port, Test)];
exit_status(Port, Command, win32) ->
  PseudoSet = "\"%errorlevel%\"==\"\"",
  Test1 = ["if ", PseudoSet, " if errorlevel 1 echo \"", 
           ?EXIT_STATUS, ":1:", Command, "\""],
  Test2 = ["if not ", PseudoSet, " if not %errorlevel%==0 echo \"", 
           ?EXIT_STATUS, ":%errorlevel%:", Command, "\""],
  [send_command(Port, Test1), send_command(Port, Test2)].
  
% Create a lock file.
lock(Port, File) -> {OS, _} = os:type(), lock(Port, File, OS).

lock(Port, File, unix) ->
  send_command(Port, io_lib:format("touch \"~s.lock\"", [File]));
lock(Port, File, win32) ->
  WinFile = pose_file:winname(File),
  send_command(Port, io_lib:format("echo touch > \"~s.lock\"", [WinFile])).

% Remove a lock file, via a command sent to external shell.
unlock(Port, File) -> {OS, _} = os:type(), unlock(Port, File, OS).

unlock(Port, File, unix) ->
  send_command(Port, io_lib:format("rm \"~s.lock\"", [File]));
unlock(Port, File, win32) ->
  WinFile = pose_file:winname(File),
  send_command(Port, io_lib:format("del \"~s.lock\"", [WinFile])).

% Send a command to the external shell.
send_command(Port, Cmd) -> {OS, _} = os:type(), send_command(Port, Cmd, OS).

send_command(Port, Cmd, unix) -> send_command(Port, Cmd, unix, "\n");
send_command(Port, Cmd, win32) -> send_command(Port, Cmd, win32, "\r\n").

send_command(Port, Cmd, _OS, Eol) ->
  Command = io_lib:format("~s~s", [Cmd, Eol]),
  Port ! {self(), {command, Command}},
  lists:flatten(Command).

% Stop cygwin from nagging.
cygwin_nodosfilewarning() ->
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin).