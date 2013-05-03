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

%% @version 0.0.5
-module(pose_shell).
-version("0.0.5").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API entry points

-export([spawn/0, command/2]).

% Private exports

-export([run/1, loop/3]).

%%
%% API Functions
%%

-type shell_pid() :: pid().
-spec spawn() -> shell_pid().
%% @doc Spawn an operating system shell interface as a new process.
spawn() -> spawn_link(?MODULE, run, [self()]).

-type command() :: string().
-spec command(ShellPid :: shell_pid(), Command :: command()) -> ok.
%% @doc Send a command to an existing shell process.  Output and error output
%% are returned as `stdout' and `stderr' messages.
%% @end
command(ShellPid, Command) -> ShellPid ! {command, self(), Command}, ok.

%%
%% Local Functions
%%

% @private Internal callback.
run(Caller) ->
  IO = ?IO(Caller), ENV = ?ENV, ?INIT_POSE,
  {OS, _} = os:type(), run(Caller, OS).

% Configure operating-system specific shell.
run(Caller, win32) -> 
  cygwin_nodosfilewarning(),
  run(Caller, win32, pose_file:trim(os:cmd("echo %ComSpec%")));
run(Caller, unix) -> run(Caller, unix, "/bin/sh"). 

% Spawn a shell executable, and start loop listening to same.
run(Caller, _OS, Shell) ->
  Options = [exit_status, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  case loop(Caller, Port, undef) of
    {error, Reason} -> exit({error, {shell, Reason}});
    ok              -> exit(ok)
  end.

% @private exported for fully-qualified calls.
loop(Caller, Port, Monitor) ->
  receive
%    {Port, {exit_status, N}}    -> do_port_exit(N);
    {Port, {data, Line}}        -> do_stderr(Caller, Port, Monitor, Line);
    {'EXIT', Port, epipe}       -> ok;
    {'EXIT', Monitor, ok}       -> loop(Caller, Port, undef);
    {'EXIT', ExitPid, Reason}   -> do_exit(Caller, Port, Monitor, ExitPid, Reason);
    {stdout, Monitor, Line}     -> do_stdout(Caller, Port, Monitor, Line);
    {command, Caller, Command} when Monitor == undef  
                                -> do_command(Caller, Port, Monitor, Command);
    Noise when Monitor == undef -> ?DEBUG("~s: noise: ~p~n", [?MODULE, Noise]),
                                   ?MODULE:loop(Caller, Port, Monitor) 
  end.

% Handle exit messages from processes.
do_exit(_Caller, _Port, Monitor, ExitPid, {error, Reason}) ->
  if ExitPid == Monitor -> {error, {cmd_stdout, Reason}};
     true               -> {error, {io_lib:format("~p", [ExitPid]), Reason}}
  end;
do_exit(Caller, Port, Monitor, ExitPid, Status) -> 
  ?DEBUG("~p saw ~p exit: ~p~n", [?MODULE, ExitPid, Status]),
  loop(Caller, Port, Monitor).
  
% Stderr messages via redirect to stdout -- forward to caller.
do_stderr(Caller, Port, Monitor, Line) ->
  Caller ! {stderr, self(), unix_eol(Line)},
  loop(Caller, Port, Monitor).

% Stdout messages via a redirect to a temp file -- forward to caller.
do_stdout(Caller, Port, _Monitor, eof) -> loop(Caller, Port, undef);
do_stdout(Caller, Port, Monitor, Line) ->
  Caller ! {stdout, self(), unix_eol(Line)},
  loop(Caller, Port, Monitor).

% Convert DOS EOL to Unix EOL.
unix_eol(Line) -> {OS, _} = os:type(), unix_eol(Line, OS).

unix_eol(Line, unix) -> Line;
unix_eol(Line, win32) ->
  Strip = string:strip(string:strip(Line, right, $\n), right, $\n),
  io_lib:format("~s~n", [Strip]).
                  
% Get a temp file to receive command's standard output channel.
do_command(Caller, Port, Monitor, Command) ->
  case pose_os:get_temp_file() of
    {error, Reason} -> {error, {temp_file, Reason}};
    {ok, Temp}      -> Strip = string:strip(Command, right, $\n),
                       do_command(Caller, Port, Monitor, Strip, Temp)
  end.

% Send command to external shell process, wrapping it with a lock file.
do_command(Caller, Port, undef, Command, Temp) ->
  lock(Port, Temp),
  Monitor = pose_shout:monitor(Temp),
  send_command(Port, io_lib:format("~s > \"~s\"", [Command, Temp])),
  unlock(Port, Temp),
  loop(Caller, Port, Monitor).

% Create a lock file.  This can be done within Erlang.
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
  Port ! {self(), {command, io_lib:format("~s~s", [Cmd, Eol])}}.

% Final return value upon external shell exit.
do_port_exit(0) -> ok;
do_port_exit(N) -> {error, {exit_status, N}}.
  
% Stop cygwin from nagging.
cygwin_nodosfilewarning() ->
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin).