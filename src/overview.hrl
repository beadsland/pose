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

%% @doc This is the POSIX-like interface emulation for use with the
%% <a href="http://github.com/beadsland/nosh">`nosh'</a> Bourne shell
%% emulator and `nosh'-compatible projects.
%%
%% <ul>
%% <li> {@section Installation} </li>
%% <li> {@section Just-in-Time Packaging} </li>
%% <li> {@section Standard I/O} </li>
%% <li> {@section Macros} </li>
%% </ul>
%%
%% ==Installation==
%%
%% `pose' should be included in any `pose'-compatible project as a project
%% dependency via `rebar.config':
%%
%% <pre>
%% {deps, [
%%    {pose, ".*",
%%      {git, "git://github.com/beadsland/pose", {branch, master}}}
%%   ]}
%% </pre>
%%
%% Any module that uses `pose' macros should then include the `pose'
%% interface header file:
%%
%% <pre>
%% %-define(debug, true).
%% -include_lib("pose/include/interface.hrl").
%% </pre>
%%
%% (The `debug' macro definition can be uncommented when `pose' debugging
%% output is to be included on `stderr'.)
%%
%% ==Just-in-Time Packaging==
%%
%% The `pose' project provides for just-in-time packaging (JITP) as part of
%% the functionality of {@link pose_code}.  Packages were an experimental 
%% feature of Erlang/OTP officially removed as of R16A01.  This project and 
%% projects dependent on it are therefore developed to run under
%% <a href="http://www.erlang.org/download_release/14">Erlang/OTP R15B01</a>,
%% but should be compatible with later releases.  
%%
%% When fully implemented, `pose' JITP will allow for the arbitrary loading
%% and concurrent operation of likenamed modules that participate in the 
%% `pose' architecture.  This will allow for seamless testing and comparison 
%% of derivative and variant code within the same runtime system.  
%% Meanwhile, as packages are only assigned to `pose'-compatible modules at 
%% compile time, existing Erlang development tools need know nothing about 
%% packages to work with `pose'-compatible modules, nor need packages be 
%% supported when such code is put into production.
%%
%% ==Standard I/O==
%%
%% Processes using the `pose' interface simulate POSIX-style
%% standard I/O streams using the `#std{}' record.  The convention is to
%% pass this record as the first parameter to functions performing or
%% implementing I/O functionality as `IO', as in:
%%
%% <pre>
%% loop(IO, ...)
%% </pre>
%%
%% Each I/O stream is a process that sends (in the case of `stdin') or
%% receives (in the case of `stdout' and `stderr') messages, such that
%% message queues simulate POSIX buffers.  The pids of each standard I/O
%% process can be addressed as follows:
%%
%% <table>
%% <tr><td> Standard input </td> <td> `IO#std.in' </td></tr>
%% <tr><td> Standard output </td> <td> `IO#std.out' </td></tr>
%% <tr><td> Standard error </td> <td> `IO#std.err' </td></tr>
%% </table>
%%
%% Messages from and to `pose' processes are of the form
%% `{Tag, SendingPid, Data}' and may be tagged as follows.
%%
%% <table>
%% <tr><td> `stdout' </td>
%% <td> A `string()' for printing by the console process.
%%      </td></tr>
%% <tr><td> `stderr' </td>
%% <td> A `string()' for printing as an error message by the console
%%      process. </td></tr>
%% <tr><td> `debug' </td>
%% <td> A `string()' for printing as an debug message by the console process.
%%      </td></tr>
%% <tr><td> `erlout' </td>
%% <td> An `atom()' or `tuple()' for pretty printing by the console process.
%%      </td></tr>
%% <tr><td> `erlerr' </td>
%% <td> An `atom()' or `tuple()' for pretty printing as an error message
%%      by the console process. </td></tr>
%% </table>
%%
%% ==Macros==
%%
%% The `pose' interface provides a number of macros for use by
%% `pose'-compatible modules.
%%
%% ===Process Run===
%%
%% <table>
%% <tr><td> `?INIT_POSE' </td>
%% <td> Initialize the `pose' interface.  Must be called by entry-function
%%      of any `pose'-compatible process. </td></tr>
%% <tr><td> `?IO(Pid :: pid()) -> #std{}' </td>
%% <td> Create a `pose' I/O record, setting each of `stdin', `stdout' and
%%      `stderr' to Pid </td></tr>
%% <tr><td> `?IO(In :: pid(), Out :: pid(), Err :: pid()) -> #std{}' </td>
%% <td> Create a `pose' I/O record. </td></tr>
%% <tr><td width="30%"> `?IO(In :: pid(), Out :: pid(), Err :: pid(),
%%      Echo :: boolean()) -> #std{}' </td>
%% <td> <i>Deprecated.</i>  Create a `pose' IO record, setting the echo
%%      flag.  Setting the flag to `true' indicates that the process run
%%      with this IO record should echo `stdin' back to `stdout'.  Defaults
%%      to false. </td></tr>
%% <tr><td> `?ARG(Command :: command(), Values :: [any()]) -> #arg{}'</td>
%% <td> Create a `pose' Arg record. </td></tr>
%% <tr><td> `?ARG(Command :: command()) -> #arg{}'</td>
%% <td> Create a `pose' Arg record, with no arguments. </td></tr>
%% <tr><td> `?ARGV(N) -> any()' </td>
%% <td> Return argument <i>N</i>.  Argument 0 corresponds to the name of
%%      the command run to start the current process.  Only available
%%      in functions that receive an `ARG' parameter. </td></tr>
%% <tr><td> `?ENV -> #env{}' </td>
%% <td> Create a `pose' Env record, setting it to hold the global
%%      environment values of the current process. </td></tr>
%% </table>
%%
%% ===Output and Error Output===
%%
%% <table>
%% <tr><td> `?STDOUT(Format :: format(), What :: list())' </td>
%% <td> Send a `io_lib:format/2` formatted `string()' message to `stdout'
%%      process.
%%      <i>Only available to functions with an IO parameter.</i></td></tr>
%% <tr><td> `?STDOUT(What :: string() | tuple() | atom())' </td>
%% <td> Send either a `string()' message to `stdout' process or an `erlout'
%%      tagged message to the same process.
%%      <i>Only available to functions with an IO parameter.</i></td></tr>
%% <tr><td> `?STDERR(Format :: format(), What :: list())' </td>
%% <td> Send a `io_lib:format/2` formatted `string()' message to `stderr'
%%      process.
%%      <i>Only available to functions with an IO parameter.</i></td></tr>
%% <tr><td> `?STDERR(What :: string() | tuple() | atom())' </td>
%% <td> Send either a `string()' message to `stderr' process or an `erlerr'
%%      tagged message to the same process.
%%      <i>Only available to functions with an IO parameter.</i></td></tr>
%% <tr><td> `?ERLERR_FORMAT(What :: any()) -> string()' </td>
%% <td> Format `erlerr' type message data as an easy-to-read string for
%%      printing to console. </td></tr>
%% <tr><td> `?DEBUG(Format :: format(), What :: list())' </td>
%% <td> If `debug' macro is `true', send an `io_lib:format/2' formatted,
%%      `debug' tagged message to `stderr' process.  Otherwise, quietly
%%      do nothing. <i>Does not require IO parameter.</i></td></tr>
%% <tr><td> `?DEBUG(What :: string())' </td>
%% <td> If `debug' macro is `true', send a `debug' tagged message to
%%      `stderr' process.  Otherwise, quietly do nothing.
%%      <i>Does not require IO parameter.</i></td></tr>
%% </table>
%%
%% @end