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

%% @doc OS-related utility functions for use by {@link pose_file}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.2
-module(pose_os).
-version("0.0.2").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

-export([get_temp_file/0, get_temp_dir/0, shell_exec/1]). 

%%
%% API Functions
%%

-spec get_temp_file() -> file:filename().
%% @doc Get a uniquely named temporary file name.
get_temp_file() ->
  {A,B,C}=now(), N=node(),
  lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])).

-spec get_temp_dir() -> file:filename() | {error, file:posix()}.
%% @doc Get system temporary directory.
get_temp_dir() ->
  case os:type() of
    {unix, _}   -> "/tmp";
    {win32, _}  -> get_temp_dir(["TEMP", "TMP"])
  end.

get_temp_dir([]) ->
  Temp = "c:\\Temp",
  case filelib:ensure_dir(Temp) of
    {error, Reason} -> {error, {Temp, Reason}};
    ok              -> Temp
  end;
get_temp_dir([First | Rest]) ->
  case os:getenv(First) of
    false   -> get_temp_dir(Rest);
    Temp    -> Temp
  end.



shell_exec(Command) -> shell_exec(Command, os:type()). 

shell_exec(Command, {win32, _}) -> 
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin),  % Keep Cygwin quiet about DOS paths.
  shell_exec(Command, pose_file:trim(os:cmd("echo %ComSpec%")), "/C");
shell_exec(Command, {unix, _}) -> shell_exec(Command, "/bin/sh", "-c").

shell_exec(Sequence, Shell, COpt) ->
  Temp = filename:join(get_temp_dir(), get_temp_file()),
  Command = io_lib:format("~s > ~s", [Sequence, Temp]),
  ?DEBUG("~s~n", [Command]),
  Args = {args, [io_lib:format("~s \"~s\"", [COpt, Command])]},
  Options = [exit_status, Args, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  shell_loop(Port, Temp, []).

% Loop through error output, and read in input following exit.
shell_loop(Port, Temp, Errors) ->
  receive
    {Port, {data, [First | Rest]}}                      ->
      Line = [string:to_lower(First) | Rest],
      CleanLine = string:strip(pose_file:trim(Line), right, $.),
      NewErrors = [CleanLine | Errors],
      shell_loop(Port, Temp, NewErrors);
    {Port, {exit_status, 0}} when Errors==[]            ->
      {ok, pose_file:trim(os:cmd("cat " ++ Temp))};
    {Port, {exit_status, 0}}                            ->
      {error, tuple_nest(Errors)};
    {Port, {exit_status, N}}                            ->
      {error, {exit_status, {N, tuple_nest(Errors)}}}
  end.

tuple_nest([First | []]) -> First;
tuple_nest([First | Rest]) -> {First, tuple_nest(Rest)}.