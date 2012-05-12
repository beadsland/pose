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

%% @doc This is the POSIX-like interface emulation for use with the
%% <a href="http://github.com/beadsland/nosh">`nosh'</a> Bourne shell
%% emulator and `nosh'-compatible projects.
%%
%% <ul>
%% <li> {@section Installation} </li>
%% <li> {@section Standard I/O} </li>
%% <li> {@section Macros} </li>
%% </ul>
%%
%% ==Installation==
%%
%% `pose' should be included in any `nosh'-compatible project as a project
%% dependency via `rebar.config':
%%
%% <blockquote>
%% {deps, [
%%    {pose, ".*",
%%      {git, "git://github.com/beadsland/pose", {branch, master}}}
%%   ]}
%% </blockquote>
%%
%% Any module that uses `pose' macros should then include the `pose'
%% interface header file:
%%
%% <blockquote>
%% %-define(debug, true).<br />
%% -include("pose/include/interface.hrl").
%% </blockquote>
%%
%% (The `debug' macro definition can be uncommented when `pose' debugging
%% output is to be included on `stderr'.)
%%
%% Any application using the `pose' interface must initialize `pose'
%% with the `?INIT_POSE' macro before other macros can be used.
%%
%% Any process using the debugging feature must additionally call the
%% `?INIT_DEBUG' macro.
%%
%% ==Standard I/O==
%%
%% Processes using the `pose' interface simulate POSIX-style
%% standard I/O streams using the `#std{}' record.  The convention is to
%% pass this record as the first parameter to functions performing or
%% implementing I/O functionality as `IO', as in:
%%
%% <blockquote>
%% loop(IO, ...)
%% </blockquote>
%%
%% Each I/O stream is a process that sends (in the case of `stdin') or
%% receives (in the case of `stdout' and `stderr') messages, such that
%% message queues simulate POSIX buffers.  The pids of each standard I/O
%% process can be addressed as follows:
%%
%% <dl>
%% <dt> Standard input </dt> <dd> `IO#std.in' </dd>
%% <dt> Standard output </dt> <dd> `IO#std.out' </dd>
%% <dt> Standard error </dt> <dd> `IO#std.err' </dd>
%% </dl>
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
%% <table>
%% <tr><td> `?INIT_POSE' </td>
%% <td> Initialize the `pose' interface.  Must be called when any
%%      pose-compatible application starts. </td></tr>
%% <tr><td> `?INIT_DEBUG' </td>
%% <td> Initialize the `pose' debug stream.  Must be called at the
%%      beginning of any pose-compatible process. </td></tr>
%% <tr><td> `?IO(Pid :: pid())' </td>
%% <td> Create a `pose' I/O record, setting each of `stdin', `stdout' and
%%      `stderr' to Pid </td></tr>
%% </table>
%%
%% <i>Draft documentation to be finished tomorrow.</i>
%%
%% @end