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

%% @version 0.1.10
-module(pose_file).
-version("0.1.10").

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
-export([can_read/1, can_write/1, last_modified/1, size/1]).

% Build environment
-export([find_parallel_folder/3]).

% Canonical paths
-export([realname/1, realname/2]).

% Temporary folder and files
-export([tempname/0, tempname/1, tempdir/0]). 

% Utility functions
-export([trim/1]).

%-export_type([info_error_atom/0]).

%%
%% API Functions
%%

%%%
% File property functions
%%%

-type info_error_atom() :: file:posix() | badarg.
-type file_info_reason() :: {file:name_all(), info_error_atom()}.
-type file_info_error() :: {error, file_info_reason()}.
-type permissions_return() :: boolean() | file_info_error().
-spec can_write(Filename :: file:name_all()) -> permissions_return().
%% @doc Test if file or directory is writeable.
can_write(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> can_write(Filename, FileInfo);
    {error, enoent} -> Dir = filename:dirname(Filename),
                       filelib:is_dir(Filename) andalso can_write(Dir);
    {error, Reason} -> {error, {Filename, Reason}}
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
    {error, Reason} -> {error, {Filename, Reason}}
  end.

can_read(_Filename, FileInfo) ->
  case FileInfo#file_info.access of
    read        -> true;
    read_write  -> true;
    _Else       -> false
  end.

-type datestamp_value() :: calendar:date_time() | nofile.
-type datestamp_return() :: {ok, datestamp_value()} | file_info_error().
-spec last_modified(Filename :: file:name_all()) -> datestamp_return().
%% @doc Get last date and time file last modified.
last_modified(Filename) ->
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> {ok, FileInfo#file_info.mtime};
    {error, enoent} -> {ok, nofile};
    {error, Reason} -> {error, {Filename, Reason}}
  end.

-type size_value() :: integer() | nofile.
-type size_return() :: {ok, size_value()} | file_info_error().
-spec size(Filename :: file:name_all()) -> size_return().
%% @doc Get current size of file.
size(Filename) -> 
  case file:read_file_info(Filename) of
    {ok, FileInfo}  -> {ok, FileInfo#file_info.size};
    {error, enoent} -> {ok, nofile};
    {error, Reason} -> {error, {Filename, Reason}}
  end.

%%%
% Build environment
%%%

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
%% @doc Ascend absolute path of file relative to current working directory, to 
%% obtain its canonical system path.
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
%% @doc Ascend absolute path of a file relative to a given directory, to obtain 
%% its canonical system path.
%% @end
realname(File, Dir) when is_binary(File) -> realname(binary_to_list(File), Dir);
realname(File, Dir) -> {OS, _} = os:type(), realname(File, Dir, OS).

% Determine absolute path, dealing as necessary with UNC paths.
realname([First | [Second | Rest]], _Dir, win32) when First==$\\, Second==$\\;
                                                      First==$/, Second==$/ ->
  do_realname(unc, [First | [Second | Rest]]);
realname(File, [First | [Second | Rest]], win32) when First==$\\, Second==$\\;
                                                      First==$/, Second==$/ ->
  do_realname(unc, filename:absname(File, [First | [Second | Rest]]));
realname(File, Dir, OS) -> do_realname(OS, filename:absname(File, Dir)).

% Tokenize the rest of our path in preparation for walking directories in shell.
do_realname(OS, File) ->
  [_Last | RevPath] = lists:reverse(string:tokens(File, "\\/")),
  do_realname(OS, File, lists:reverse(RevPath)).

% Determine first shell command as a function of operating system.
do_realname(unc, File, [Server | [Share | Path]]) ->
  Unc = io_lib:format("\\\\~s\\~s", [Server, Share]),
  case do_realname(win32, File, [Unc | Path]) of
    {error, Reason}             -> {error, Reason};
    {ok, [_L | [_C | Real]]}    -> Parts = [Server, Share, Real],
                                   {ok, io_lib:format("//~s/~s~s", Parts)}
  end;
do_realname(win32, File, [Drive | Path]) ->
  do_realname(win32, File, Path, [io_lib:format("pushd ~s & cd \\", [Drive])]);
do_realname(unix, File, Path) -> do_realname(unix, File, Path, ["cd /"]).

% Generate change directory sequence, and specify shell specifics.
do_realname(win32, File, [], Cmds) -> short_realname(File, ["chdir\s" | Cmds]);
do_realname(unix, File, [], Cmds) -> short_realname(File, ["pwd" | Cmds]);
do_realname(OS, File, [Folder | Path], Cmds) ->
  case re:run(Folder, "[^a-zA-Z0-9_]") of
    {match, _}  -> Cmd = io_lib:format("cd \"~s\"", [Folder]);
    nomatch     -> Cmd = io_lib:format("cd ~s", [Folder])
  end,
  do_realname(OS, File, Path, [Cmd | Cmds]).

% Pass commands to a short-circuiting operating system script.
short_realname(File, Cmds) ->
  case pose_short:script(string:join(lists:reverse(Cmds), "\n")) of
    {error, Reason} -> {error, {short, Reason}};
    {ok, [Result]}  -> AbsDir = string:strip(lists:flatten(Result), right, $\n),
                       {ok, filename:join(AbsDir, filename:basename(File))};
    {ok, Results}   -> {error, {excessive_results, {Results}}}
  end.
 
%%%
% Temporary folder and files
%%%

-type temp_file_error() :: {error, {temp_dir, file:posix()}}. 
-spec tempname() -> {ok, file:filename()} | temp_file_error().
%% @doc Get a unique name for a temporary file in the system temporary 
%% directory.
%% @end
tempname() -> 
  case tempdir() of
    {error, Reason} -> {error, {temp_dir, Reason}};
    {ok, Dir}       -> tempname(Dir)
  end.

-spec tempname(Dir :: file:filename()) -> {ok, file:filename_all()}.
%% @doc Get a unique name for a temporary file in the specified directory.
tempname(Dir) -> 
  {A,B,C}=now(), N=node(),
  File = lists:flatten(io_lib:format("~p-~p.~p.~p",[N,A,B,C])),
  {ok, filename:join(Dir, File)}.
  
-spec tempdir() -> {ok, file:filename()} | {error, file:posix()}.
%% @doc Get system temporary directory.
tempdir() ->
  case os:type() of
    {unix, _}   -> "/tmp";
    {win32, _}  -> tempdir(["TEMP", "TMP"])
  end.

tempdir([]) ->
  Temp = "c:\\Temp",
  case filelib:ensure_dir(Temp) of
    {error, Reason} -> {error, {Temp, Reason}};
    Temp            -> {ok, Temp}
  end;
tempdir([First | Rest]) ->
  case os:getenv(First) of
    false       -> tempdir(Rest);
    Temp        -> {ok, Temp}
  end.

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