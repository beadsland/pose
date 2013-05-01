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
run(Caller) -> {OS, _} = os:type(), run(Caller, OS).

% Configure operating-system specific shell.
run(Caller, win32) -> 
  cygwin_nodosfilewarning(),
  run(Caller, win32, pose_file:trim(os:cmd("echo %ComSpec%")));
run(Caller, unix) -> run(Caller, unix, "/bin/sh"). 

% Spawn a shell executable, and start loop listening to same.
run(Caller, OS, Shell) ->
  Options = [exit_status, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  case loop(Caller, OS, Port) of
    {error, Reason} -> exit({error, {shell, Reason}});
    ok              -> exit(ok)
  end.

% @private exported for fully-qualified calls.
loop(Caller, OS, Port) ->
  receive
    {Port, {exit_status, N}}    -> do_port_exit(N);
    {command, Caller, Command}  -> do_command(Caller, OS, Port, Command);
    Noise                       -> ?DEBUG("~s: noise: ~p~n", [?MODULE, Noise]),
                                   ?MODULE:loop(Caller, OS, Port)
  end.

% Get a temp file to receive command's standard output channel.
do_command(Caller, OS, Port, Command) ->
  case pose_os:get_temp_file() of
    {error, Reason} -> {error, {temp_file, Reason}};
    {ok, Temp}      -> do_command(Caller, OS, Port, Command, Temp)
  end.

% Configure operating-system specific eol.
do_command(Caller, unix, Port, Command, Temp) ->
  do_command(Caller, unix, Port, Command, Temp, "\n");
do_command(Caller, win32, Port, Command, Temp) ->
  do_command(Caller, win32, Port, Command, Temp, "\r\n").

% Send command to external shell process.
do_command(Caller, OS, Port, Command, Temp, Eol) ->
  Strip = string:strip(Command, right, $\n),
  RedirCmd = io_lib:format("~s > ~s~s", [Strip, Temp, Eol]),
  Port ! {self(), {command, RedirCmd}},
  loop(Caller, OS, Port).

% Final return value upon external shell exit.
do_port_exit(0) -> ok;
do_port_exit(N) -> {error, {exit_status, N}}.
  
% Stop cygwin from nagging.
cygwin_nodosfilewarning() ->
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin).