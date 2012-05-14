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

-import(code).
-define(INIT_POSE, init_pose, process_flag(trap_exit, true),
                   code:add_patha("deps/pose/ebin"),
                   put(debug, IO#std.err)).

% IO is first parameter to pose entry points.
-record(std, {in = self() :: pid(), out = self() :: pid(),
              err = self() :: pid(), echo = false :: boolean()}).

-define(IO(In, Out, Err, Echo), #std{in=In, out=Out, err=Err, echo=Echo}).
-define(IO(In, Out, Err), #std{in=In, out=Out, err=Err}).
-define(IO(Pid), #std{in=Pid, out=Pid, err=Pid}).

% ARG is second parameter to pose entry points.
-record(arg, {cmd = '' :: atom(), v = [] :: list()}).

-import(pose).
-define(ARG(C), ?ARG(C, [])).
-define(ARG(C, V), #arg{cmd = C, v = V}).
-define(ARGV(X), pose:argv(ARG, X)).

% ENV is third parameter to pose entry points.
-type env_prop() :: atom() | {atom(), string()}.
-record(env, {plist = [] :: [env_prop()]}).

-define(ENV, #env{}).

% STDERR and STDOUT work only in functions that receive IO parameter
-import(pose_stdio).  % May be used by packaged modules.
-define(STDERR(Format, What),
        stderr, pose_stdio:send_stderr(IO, Format, What)).
-define(STDERR(What), stderr, pose_stdio:send_stderr(IO, What)).
-define(STDOUT(Format, What),
        stdout, pose_stdio:send_stdout(IO, Format, What)).
-define(STDOUT(What), stdout, pose_stdio:send_stdout(IO, What)).

-define(FORMAT_ERLERR(What), pose_stdio:format_erlerr(What)).

% DEBUG is a special case of STDERR that does not require IO
-ifdef(debug).
-define(DEBUG(Format, What), debug, pose_stdio:send_debug(Format, What)).
-else.
-define(DEBUG(Format, What), debug, put(devnull, {Format, What})).
-endif.
-define(DEBUG(String), ?DEBUG("~s", [String])).