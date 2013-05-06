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

%% @doc Synchronously execute multiple commands under an operating system shell,
%% short-circuiting on any non-zero exit status.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.2
-module(pose_short).
-version("0.0.2").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API entry points

-export([script/1]).

% Private exports

-export([loop/3]).

%%
%% API Functions
%%

-type shell_output() :: {ok, [string()]}.
-type shell_error_item() :: atom() | pid() | port() | integer() | string().
-type shell_error_term() :: shell_error_item() | shell_error_tuple().
-type shell_error_tuple() :: {shell_error_item(), shell_error_term()}. 
-type shell_error() :: {error, shell_error_item() | shell_error_tuple()}.
-spec script(Sequence :: string()) -> shell_output() | shell_error().
%% @doc Pass a carriage-return delimited sequence of commands to an operating
%% system shell, returning either an error result on the first non-zero
%% exit status or a list of carriage-return terminated strings representing
%% output from the command sequence.
%% @end
script(Sequence) -> script(pose_shell:spawn(), string:tokens(Sequence, "\n")).

script(Shell, []) ->
  ?DEBUG("launched shell ~p~n", [Shell]),
  pose_shell:exit(Shell),
  ?MODULE:loop(Shell, [], []);
script(Shell, [Cmd | Cmds]) -> 
  pose_shell:command(Shell, Cmd), 
  script(Shell, Cmds).

%%
%% Local Functions
%%

% @private exported for fully-qualified calls.
loop(Port, Out, Err) ->
  receive
    {purging, _Pid, _Module}    -> ?MODULE:loop(Port, Out, Err);
    {'EXIT', Port, ok}          -> {ok, Out};
    {'EXIT', Port, Reason}      -> do_shell_error(Err, Reason);
    {'EXIT', ExitPid, normal}   -> ?DOEXIT, ?MODULE:loop(Port, Out, Err);
    {stdout, Port, Line}        -> ?MODULE:loop(Port, [Line | Out], Err);
    {stderr, Port, Line}        -> ?MODULE:loop(Port, Out, [Line | Err]);
    {erlerr, Port, Status}      -> do_erlerr(Port, Out, Err, Status);
    {debug, Port, Line}         -> ?DEBUG(Line), ?MODULE:loop(Port, Out, Err);
    Noise                       -> ?DONOISE, ?MODULE:loop(Port, Out, Err)
  end.

% Handle exit status passed back from command.
do_erlerr(Port, Out, Err, {exit_status, {Command, 0}}) ->
  ?DEBUG(io_lib:format("~s~n", [?FORMAT_ERLERR({Command, {exit_status, 0}})])),
  ?MODULE:loop(Port, Out, Err);
do_erlerr(_Port, _Out, Err, {exit_status, {Command, Code}}) ->
  do_shell_error(Err, {Command, {exit_status, Code}}).
  
% Handle error exit from shell.
do_shell_error(Err, Reason) ->
  {error, tuple_nest(lists:append(tuple_unnest(Reason), err_unline(Err)))}.

% Convert errout lines to error strings.
err_unline([]) -> [];
err_unline([[Initial | Line] | Lines]) ->
  Strip = string:strip(string:strip(Line, right, $\n), right, $.),
  Unline = [string:to_lower(Initial) | Strip],
  [Unline | err_unline(Lines)].

% Translate nested 2-tuples into a list.
tuple_unnest({Term, Tuple}) when is_tuple(Tuple) -> 
  [Term | tuple_unnest(Tuple)];
tuple_unnest({Term, Term2}) -> [Term, Term2].

% Translate a list into a nested 2-tuples.
tuple_nest([First | []]) -> First;
tuple_nest([First | Rest]) -> {First, tuple_nest(Rest)}.