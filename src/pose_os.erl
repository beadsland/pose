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

%% @version 0.0.4
-module(pose_os).
-version("0.0.4").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API entry points

-export([get_temp_file/0, get_temp_file/1, get_temp_dir/0, shell_exec/1]). 

% Private exports

-export([shell_loop/3, shell_loop/4]).

%%
%% API Functions
%%

%%%
% Temporary folder and unique file names
%%%

-type temp_file_error() :: {error, {temp_dir, file:posix()}}. 
-spec get_temp_file() -> {ok, file:filename()} | temp_file_error().
%% @doc Get a unique name for a temporary file in the system temporary 
%% directory.
%% @end
get_temp_file() -> 
  case get_temp_dir() of
    {error, Reason} -> {error, {temp_dir, Reason}};
    {ok, Dir}       -> get_temp_file(Dir)
  end.

-spec get_temp_file(Dir :: file:filename()) -> {ok, file:filename_all()}.
%% @doc Get a unique name for a temporary file in the specified directory.
get_temp_file(Dir) -> 
  {A,B,C}=now(), N=node(),
  File = lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])),
  {ok, filename:join(Dir, File)}.
  
-spec get_temp_dir() -> {ok, file:filename()} | {error, file:posix()}.
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
    Temp            -> {ok, Temp}
  end;
get_temp_dir([First | Rest]) ->
  case os:getenv(First) of
    false       -> get_temp_dir(Rest);
    Temp        -> {ok, Temp}
  end.

%%%
% Execution of commands under OS shell
%%%

-type shell_error_term() :: string() | integer() | atom().
-type shell_error_tuple() :: {shell_error_term(), 
                              shell_error_term() | shell_error_tuple()}.
-type shell_error() :: {error, string() | shell_error_tuple()}.
-spec shell_exec(Command :: string()) -> {ok, string()} | shell_error(). 
%% @doc Execute a command in operating system shell, capturing stdout and 
%% stderr independently.
%% @end
shell_exec(Command) -> shell_exec(Command, os:type()). 

% Stop cygwin from nagging, and identify operation system specific shell.
shell_exec(Command, {win32, _}) -> 
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin),
  shell_exec(Command, pose_file:trim(os:cmd("echo %ComSpec%")), "/C");
shell_exec(Command, {unix, _}) -> shell_exec(Command, "/bin/sh", "-c").

% Configure and spawn shell process.
shell_exec(Command, Shell, COpt) ->
  case get_temp_file() of
    {error, Reason} -> {error, {temp_file, Reason}};
    {ok, Temp}      -> shell_exec(Command, Shell, COpt, Temp)
  end.

shell_exec(Command, Shell, COpt, Temp) ->
  RedirCmd = io_lib:format("~s > ~s", [Command, Temp]),
  Args = {args, [io_lib:format("~s \"~s\"", [COpt, RedirCmd])]},
  Options = [exit_status, Args, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  shell_loop(Port, Temp, []).

% Loop through error output until shell finishes.    
shell_loop(Port, Temp, Errors) ->
  receive
    {Port, {data, [First | Rest]}}                      ->
      Line = [string:to_lower(First) | Rest],
      CleanLine = string:strip(pose_file:trim(Line), right, $.),
      shell_loop(Port, Temp, [CleanLine | Errors]);
    {Port, {exit_status, 0}} when Errors==[]            ->
      ReadPid = pose_open:read(Temp),
      ?CAPTLN(ReadPid),
      shell_loop(Port, Temp, ReadPid, []);
    {Port, {exit_status, 0}}                            ->
      {error, tuple_nest(Errors)};
    {Port, {exit_status, N}} when Errors==[]            ->
      {error, {exit_status, N}};
    {Port, {exit_status, N}}                            ->
      {error, {exit_status, {N, tuple_nest(Errors)}}};
    Noise                                               ->
      ?DEBUG("noise: ~p~n", [Noise]),
      ?MODULE:shell_loop(Port, Temp, Errors)
  end.

% Loop through standard output, clean up temporary file, and return result.
shell_loop(Port, Temp, ReadPid, Output) ->
  receive
    {'EXIT', Port, normal}      ->
      shell_loop(Port, Temp, ReadPid, Output);
    {'EXIT', ReadPid, Reason}   -> 
      case file:delete(Temp) of
        {error, Reason} -> {error, {delete, {Temp, Reason}}};
        ok              -> {ok, lists:reverse(Output)}
      end;
    {stdout, ReadPid, Line}     -> 
      ?CAPTLN(ReadPid),
      shell_loop(Port, Temp, ReadPid, [Line | Output]);
    Noise                       ->
      ?DEBUG("noise: ~p~n", [Noise]),
      ?MODULE:shell_loop(Port, Temp, ReadPid, Output)      
  end.
  
%%
%% Local Functions
%%

tuple_nest([First | []]) -> First;
tuple_nest([First | Rest]) -> {First, tuple_nest(Rest)}.