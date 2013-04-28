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

%% @doc File-related utility functions for use by {@link pose_code}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @todo spec API functions

%% @version 0.1.6
-module(pose_file).
-version("0.1.6").

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").

-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

% File properties
-export([can_read/1, can_write/1, last_modified/1]).

% Build environment
-export([get_temp_file/0, get_temp_dir/0, find_parallel_folder/3]).

% Canonical paths
-export([realname/1, realname/2]).

% Utility functions
-export([trim/1]).

-export_type([info_error_atom/0]).

%%
%% API Functions
%%

%%%
% File property functions
%%%

-type info_error_atom() :: file:posix() | badarg.
-type file_info_error() :: {file:name_all(), info_error_atom()}.
-type permissions_return() :: boolean() | {error, file_info_error()}.
-spec can_write(Filename :: file:name_all()) -> permissions_return().
%% @doc Test if file or directory is writeable.
can_write(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> can_write(Filename, FileInfo);
    {error, enoent} -> can_write(filename:dirname(Filename));
    {error, What}   -> {error, {Filename, What}}
  end.

can_write(_Filename, FileInfo) ->
  case FileInfo#file_info.access of
    write       -> true;
    read_write  -> true;
    _Else       -> false
  end.

-spec can_read(Filename :: file:name_all()) -> permissions_return().
%% @doc Test if file or directory is readable.
can_read(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> can_read(Filename, FileInfo);
    {error, enoent} -> false;
    {error, What}   -> {error, {Filename, What}}
  end.

can_read(_Filename, FileInfo) ->
  case FileInfo#file_info.access of
    read        -> true;
    read_write  -> true;
    _Else       -> false
  end.

-type date_time() :: calendar:date_time().
-type datestamp_return() :: {ok, date_time()} | {error, file_info_error()}.
-spec last_modified(Filename :: file:name_all()) -> datestamp_return().
%% @doc Get last date and time file last modified.
last_modified(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> {ok, FileInfo#file_info.mtime};
    {error, enoent} -> {ok, nofile};
    {error, What}   -> {error, {Filename, What}}
  end.

%%%
% Build environment
%%%

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

-type folder() :: nonempty_string().
-type path_string() :: nonempty_string().
-type path_list() :: {folders, [folder()]}.
-type path() :: path_string() | path_list().
-type parallel_result() :: {false, path_string()} | {true, path_string()}.
-spec find_parallel_folder(OldFlder :: folder(), NewFolder :: folder(),
                           OldPath :: path()) -> parallel_result().
%% @doc Walk absolute directory path, finding where parallel would occur.
find_parallel_folder(OldFldr, NewFldr, OldDir) when is_list(OldDir) ->
  Split = re:split(OldDir, "/", [{return, list}]),
  find_parallel_folder(OldFldr, NewFldr, {folders, Split});
find_parallel_folder(OldFldr, NewFldr, {folders, [Head | []]}) ->
  if Head == OldFldr -> {true, NewFldr}; true -> {false, Head} end;
find_parallel_folder(OldFldr, NewFldr, {folders, [Head | Tail]}) ->
%  ?DEBUG("~s(~p, ~p, {folders, [~p | Tail]})~n",
%         [?MODULE, OldFldr, NewFldr, Head]),
  case find_parallel_folder(OldFldr, NewFldr, {folders, Tail}) of
    {true, NewDir}                          ->
      {true, lists:append([Head, "/", NewDir])};
    {false, OldDir} when Head == OldFldr    ->
      {true, lists:append([NewFldr, "/", OldDir])};
    {false, OldDir}                         ->
      {false, lists:append([Head, "/", OldDir])}
  end.

%%%
% Canonical paths
%%%

-spec realname(File :: file:filename_all()) -> path_string().
%% @doc Ascend absolute directory path of file relative to current working
%% directory, to obtain its canonical system path.
%% @end
realname(File) ->
  case file:get_cwd() of
    {error, Reason} -> {error, {cwd, Reason}};
    {ok, Dir}       -> realname(File, Dir)
  end.

-type exit_status() :: integer().
-type realname_error() :: {error, {exit_status(), [string()]}}.
-type realname_result() :: path_string() | realname_error().
-spec realname(File :: file:filename_all(), Dir :: file:filename_all())
                                                        -> realname_result().
%% @doc Ascend absolute directory path of a file relative to a directory,
%% to obtain its canonical system path.
%% @end
realname(File, Dir) when is_binary(File) -> realname(binary_to_list(File), Dir);
realname([First | [Second | Rest]], _Dir) when First == $\\, Second == $\\;
                                               First == $/, Second == $/ ->
  [_Last | RevPath] = lists:reverse(string:tokens(Rest, "\\/")),
  do_realname([$/ | [$/ | Rest]], unc, lists:reverse(RevPath));
realname(File, Dir) ->
  {OS, _} = os:type(),
  AbsFile = filename:abspath(File, Dir),
  [_Last | RevPath] = lists:reverse(string:tokens(AbsFile, "\\/")),
  do_realname(File, OS, lists:reverse(RevPath)).

% Work out commands for UNC and Win32 drive paths.
% First command depends on operating system.
do_realname(File, unc, [Server | [Share | Path]]) ->
  Unc = io_lib:format("\\\\~s\\~s", [Server, Share]),
  case do_realname(File, win32, [Unc | Path]) of
    {error, Reason}             -> {error, Reason};
    {ok, [_L | [_C | Real]]}    -> Parts = [Server, Share, Real],
                                   {ok, io_lib:format("//~s/~s~s", Parts)}
  end;
do_realname(File, win32, [Drive | Path]) ->
  Cmd = io_lib:format("pushd ~s & cd \\", [Drive]),
  do_realname(File, win32, Path, [Cmd]);
do_realname(File, unix, Path) -> do_realname(File, unix, Path, ["cd /"]).

% Generate change directory sequence, and specify shell specifics.
% @todo Rewrite this to write commands to a batch/shell script file.
do_realname(File, win32, [], Cmds) ->
  Shell = trim(os:cmd("echo %ComSpec%")), COpt = "/C", Sep = " & ",

  % If invoked under Cygwin, this will keep quiet about DOS paths.
  Cygset = sets:from_list(string:tokens(os:getenv("CYGWIN"), " ")),
  Cygadd = sets:add_element("nodosfilewarning", Cygset),
  Cygwin = string:join(sets:to_list(Cygadd), " "),
  os:putenv("CYGWIN", Cygwin),

  % Trailing space required by windows shell in order to get a `pwd' result.
  do_realname(File, win32, [], ["chdir " | Cmds], Shell, COpt, Sep);
do_realname(File, unix, [], Cmds) ->
  Shell = "/bin/sh", COpt = "-c", Sep = " ; ",
  do_realname(File, unix, [], ["pwd" | Cmds], Shell, COpt, Sep);
do_realname(File, OS, [Folder | Path], Cmds) ->
  Cmd = io_lib:format("cd \"~s\"", [Folder]),
  do_realname(File, OS, Path, [Cmd | Cmds]).

% Assemble and execute commands.
do_realname(File, _OS, _, Cmds, Shell, COpt, Sep) ->
  Sequence = string:join(lists:reverse(Cmds), Sep),
  Temp = filename:join(get_temp_dir(), get_temp_file()),
  Command = io_lib:format("~s > ~s", [Sequence, Temp]),
  ?DEBUG("~s~n", [Command]),
  Args = {args, [io_lib:format("~s \"~s\"", [COpt, Command])]},
  Options = [exit_status, Args, hide, stderr_to_stdout],
  Port = open_port({spawn_executable, Shell}, Options),
  realname_loop(Port, Temp, File, []).

% Loop through error output, and read in input following exit.
realname_loop(Port, Temp, File, Errors) ->
  receive
    {Port, {data, [First | Rest]}}                      ->
      Line = [string:to_lower(First) | Rest],
      CleanLine = string:strip(pose_file:trim(Line), right, $.),
      NewErrors = [CleanLine | Errors],
      realname_loop(Port, Temp, File, NewErrors);
    {Port, {exit_status, 0}} when Errors==[]            ->
      Cat = pose_file:trim(os:cmd("cat " ++ Temp)),
      {ok, filename:join(Cat, filename:basename(File))};
    {Port, {exit_status, 0}}                            ->
      {error, tuple_nest(Errors)};
    {Port, {exit_status, N}}                            ->
      {error, {exit_status, {N, tuple_nest(Errors)}}}
  end.

tuple_nest([First | []]) -> First;
tuple_nest([First | Rest]) -> {First, tuple_nest(Rest)}.

%%%
% Exported utility functions
%%%

-spec trim(String :: string()) -> string().
%% @doc Strip whitespace characters from both ends of string.
%% @todo figure out better place for this to live
trim(String) when is_list(String) -> trim(String, forward).

% Strip whitespace characters from both ends of a string.
trim([$\s | String], Direction) -> trim(String, Direction);
trim([$\t | String], Direction) -> trim(String, Direction);
trim([$\n | String], Direction) -> trim(String, Direction);
trim([$\r | String], Direction) -> trim(String, Direction);
trim(String, backward) -> lists:reverse(String);
trim(String, forward) -> trim(lists:reverse(String), backward).

%%
%% Local Functions
%%