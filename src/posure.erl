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

%% @doc Posure package import checker for pose.  Checks pose-compatible
%% command modules for unimported library modules.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @version 0.1.2

-define(module, posure).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.2").

%%
%% Include files
%%

%-define(debug, true).
-include("interface.hrl").

-include("macro.hrl").

-import(filename).
-import(re).
-import(re).
-import(ets).

%%
%% Exported Functions
%%

-export([start/0, run/3]).

-export([loop/2]).  % hidden

%%
%% API Functions
%%

-spec start() -> ok | notsure | {error, {atom(), file:filename()}}.
%% @doc Start posure package import check as a blocking function.
%% All results are written to standard output.
%% @end
start() ->
  posure_test:hello(),
  IO = ?IO(self()),
  RunPid = spawn_link(?MODULE, run, [IO, ?ARG(?MODULE), ?ENV]),
  ?MODULE:loop(IO, RunPid).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% @doc Start posure package import check as a
%% <a href="http://github.com/beadsland/pose">pose</a> process.
%% @end
%% @todo Error on direct calls to non-submodule commands
run(IO, _ARG, _ENV) ->
  ?INIT_POSE,
  ?STDOUT("Running Posure ~s package import checker~n", [?VERSION(?MODULE)]),
  Src = filename:absname("src"),
  Pattern = lists:append(Src, "/*.erl"),
  Source = filelib:wildcard(Pattern),
  case slurp_pose_sources(Source) of
    {error, What} -> ?STDERR("posure: ~s~n", ?FORMAT_ERLERR(What)),
                     exit({error, What});
    {ok, Slurps}  -> warn_nonimported_modules(IO, Slurps)
  end.

%%
%% Local Functions
%%

warn_nonimported_modules(IO, Slurps) ->
  Keys = proplists:get_keys(Slurps),
  ?DEBUG("slurps: ~p~n", [Keys]),
  Commands = [get_command_name(X) || X <- Keys],
  ?DEBUG("pose commands: ~p~n", [Commands]),
  warn_nonimported_modules(IO, Commands, Slurps).

warn_nonimported_modules(IO, _Commands, []) ->
  ?STDOUT("Quite sure!\n"),
  exit(ok);
warn_nonimported_modules(IO, Commands, [{File, Data} | Tail]) ->
  ThisCommand = get_command_name(File),
  Imports = get_imported_modules(Data),
  ?DEBUG("imports: ~p~n", [Imports]),
  Called = get_called_modules(Data),
  ?DEBUG("called: ~p~n", [Called]),
  Unimported = lists:subtract(lists:subtract(Called, Imports), Commands),
  [send_unimported_error(IO, ThisCommand, X) || X <- Unimported],
  BadDirect = [X || X <- Called, lists:member(X, Commands),
                    is_submodule(X, ThisCommand) == false],
  [send_baddirect_error(IO, ThisCommand, X) || X <- BadDirect],
  Noncalled = lists:subtract(Imports, Called),
  [send_noncalled_error(IO, ThisCommand, X) || X <- Noncalled],
  case length(Unimported) of
    0       -> warn_nonimported_modules(IO, Tail);
    _Else   -> ?STDOUT("Not so sure.\n"),
               exit(notsure)
  end.

is_submodule(X, Y) ->
  Test1 = lists:prefix(X ++ "_", Y),
  Test2 = lists:prefix(Y ++ "_", X),
  if Test1; Test2   -> true;
     true           -> false
  end.

send_unimported_error(IO, Command, Module) ->
  ?STDERR("~s: calls unimported module '~s'~n", [Command, Module]).

send_noncalled_error(IO, Command, Module) ->
  ?STDERR("~s: imports unused module '~s'~n", [Command, Module]).

send_baddirect_error(IO, Command, Module) ->
  ?STDERR("~s: calls pose command module '~s'~n", [Command, Module]).

get_command_name(File) ->
  {ok, MP} = re:compile("\\/([^\\/]+)\\.erl$"),
  case re:run(File, MP, [{capture, [1], list}]) of
    nomatch                 -> File;
    {match, [Command]}      -> Command
  end.

get_imported_modules(Data) ->
  {ok, MP} = re:compile("-import\\(([^)]+)\\)", [multiline]),
  case re:run(Data, MP, [global, {capture, [1], list}]) of
    nomatch             -> [];
    {match, Imports}    -> [lists:nth(1, X) || X <- Imports]
  end.

get_called_modules(Data) ->
  {ok, MP} = re:compile("[\\s\\[\\{\\(\\,]([a-z_]+)\\:[a-z_]+\\(",
                        [multiline]),
  case re:run(Data, MP, [global, {capture, [1], list}]) of
    nomatch             -> [];
    {match, Imports}    -> List = [lists:nth(1, X) || X <- Imports],
                           sets:to_list(sets:from_list(List))
  end.

%%%
% Slurp pose sources
%%%

slurp_pose_sources([]) -> {ok, []};
slurp_pose_sources([Head | Tail]) ->
  case file:read_file(Head) of
    {error, What}   -> {error, {What, Head}};
    {ok, Data}      -> slurp_pose_sources(Tail, {Head, Data})
  end.

slurp_pose_sources(Tail, {Head, Data}) ->
  {ok, MP} = re:compile("^-package\\(default\\)\\.", [multiline]),
  case re:run(Data, MP, [{capture, none}]) of
    nomatch -> slurp_pose_sources(Tail);
    match   -> case slurp_pose_sources(Tail) of
                 {error, What}  -> {error, What};
                 {ok, Slurps}   -> {ok, [{Head, Data} | Slurps]}
               end
  end.

%%%
% Start loop
%%%

% @hidden Export to allow for hotswap.
loop(IO, RunPid) ->
  receive
    {purging, _Pid, _Mod}       -> ?MODULE:loop(IO, RunPid);
    {'EXIT', RunPid, Reason}    -> Reason;
    {MsgTag, RunPid, Line}      -> do_output(MsgTag, Line),
                                   ?MODULE:loop(IO, RunPid);
    Noise                       -> do_noise(Noise),
                                   ?MODULE:loop(IO, RunPid)
  end.

% Handle stderr and stdout messages.
do_output(MsgTag, Output) ->
  case MsgTag of
    stdout  -> io:format("~s", [Output]);
    erlout  -> io:format("~p: data: ~p~n", [?MODULE, Output]);
    erlerr  -> Erlerr = ?FORMAT_ERLERR(Output),
               io:format(standard_error, "** ~s~n", [Erlerr]);
    stderr  -> io:format(standard_error, "** ~s", [Output]);
    debug   -> io:format(standard_error, "-- ~s", [Output])
  end.

% Handle message queue noise.
do_noise(Noise) ->
  io:format(standard_error, "noise: ~p ~p~n", [Noise, self()]).
