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
%% Copyright 2012, 2013 Beads D. Land-Trujillo.  All Rights Reserved.
%% -----------------------------------------------------------------------
%% CDDL HEADER END

-ifdef(package).
-import(code).
-endif.

-define(INIT_POSE, init_pose,
                   process_flag(trap_exit, true),
                   put(debug, IO#std.err),
                   put(env, ENV#env.plist),
                   pose:init_path()
       ).

% IO is first parameter to pose entry points.
-record(std, {in = self() :: pid(), out = self() :: pid(),
              err = self() :: pid(),
              stop = false :: boolean(), echo = false :: boolean()
      }).

-define(IO(In, Out, Err, Stop, Echo),
                     #std{in=In, out=Out, err=Err, stop=Stop, echo=Echo}).
-define(IO(In, Out, Err, Stop), #std{in=In, out=Out, err=Err, stop=Stop}).
-define(IO(In, Out, Err), #std{in=In, out=Out, err=Err}).
-define(IO(Pid, Stop), #std{in=Pid, out=Pid, err=Pid, stop=Stop}).
-define(IO(Pid), #std{in=Pid, out=Pid, err=Pid}).

% ARG is second parameter to pose entry points.
-record(arg, {cmd = '' :: atom(), v = [] :: list()}).

-ifdef(package).
-import(pose).
-endif.

-define(ARG(C), ?ARG(C, [])).
-define(ARG(C, V), #arg{cmd = C, v = V}).
-define(ARGV(X), pose:argv(ARG, X)).

% ENV is third parameter to pose entry points.
-type env_prop() :: atom() | {atom(), string()}.
-record(env, {plist = [] :: [env_prop()]}).

-define(ENV, #env{}).

% STDERR and STDOUT work only in functions that receive IO parameter
-ifdef(package).
-import(pose_stdio).  % May be used by packaged modules.
-endif.

-define(CAPTLN, ?CAPTLN(IO#std.in)).
-define(CAPTLN(IOPid), IOPid ! {stdin, self(), captln}).

-define(STDOUT(Format, What),
        stdout, pose_stdio:send_stdout(IO, Format, What)).
-define(STDOUT(Output), stdout, pose_stdio:send_stdout(IO, Output)).

-define(STDERR(Format, What),
        stderr, pose_stdio:send_stderr(IO, Format, What)).
-define(STDERR(Output), stderr, pose_stdio:send_stderr(IO, Output)).

-define(FORMAT_ERLERR(What), pose_stdio:format_erlerr(What)).

% DEBUG is a special case of STDERR that does not require IO
-ifdef(debug).
-define(DEBUG(Format, What), debug, pose_stdio:send_debug(Format, What)).
-define(DEBUG(Output), debug, pose_stdio:send_debug(Output)).
-else.
-define(DEBUG(Format, What), debug, put(devnull, {Format, What})).
-define(DEBUG(Output), debug, put(devnull, Output)).
-endif.

% Macros for concice receive loops.
-define(DOEXIT, ?DEBUG("~s ~p: saw ~p exit~n", [?MODULE, self(), ExitPid])).
-define(DONOISE, ?DEBUG("~s ~p: noise: ~p~n", [?MODULE, self(), Noise])).