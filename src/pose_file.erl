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
%% Copyright 2012 Beads D. Land-Trujillo.  All Rights Reserved
%% -----------------------------------------------------------------------
%% CDDL HEADER END

%% @doc File-related utility functions for use by {@link pose_code}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

%% @todo spec API functions

-module(pose_file).

%%
%% Include files
%%

-include_lib("kernel/include/file.hrl").

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

-include("macro.hrl").

%%
%% Exported Functions
%%

% File Properties
-export([can_read/1, can_write/1, last_modified/1, find_parallel_folder/3]).

-export_type([filename/0, file_err/0]).

%%
%% API Functions
%%


%%%
% File property functions
%%%

-type filename() :: nonempty_string().
-type file_err() :: {atom(), filename()}.
-spec can_write(Filename :: filename()) -> boolean() | {error, file_err()}.
%% @doc Test if file or directory is writeable.
can_write(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  ->
            case FileInfo#file_info.access of
                write       -> true;
                read_write  -> true;
                _Else       -> false
            end;
        {error, enoent} ->
            true;   % File does not exist, so is writeable if directory is.
        {error, What}   ->
            {error, {What, Filename}}
    end.

-spec can_read(Filename :: filename()) -> boolean() | {error, file_err()}.
%% @doc Test if file or directory is readable.
can_read(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  ->
            case FileInfo#file_info.access of
                read        -> true;
                read_write  -> true;
                _Else       -> false
            end;
        {error, enoent} -> false;
        {error, What}   -> {error, {What, Filename}}
    end.

-type date_time() :: calendar:date_time().
-type last_mod_rtn() :: {ok, date_time()} | {error, file_err()}.
-spec last_modified(Filename :: filename()) -> last_mod_rtn().
%% @doc Get last date and time file last modified.
last_modified(Filename) ->
    case file:read_file_info(Filename) of
        {ok, FileInfo}  -> {ok, FileInfo#file_info.mtime};
        {error, enoent} -> {ok, nofile};
        {error, What}   -> {error, {What, Filename}}
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
  ?DEBUG("~s(~p, ~p, {folders, [~p | Tail]})~n",
         [?MODULE, OldFldr, NewFldr, Head]),
  case find_parallel_folder(OldFldr, NewFldr, {folders, Tail}) of
    {true, NewDir}                          ->
      {true, lists:append([Head, "/", NewDir])};
    {false, OldDir} when Head == OldFldr    ->
      {true, lists:append([NewFldr, "/", OldDir])};
    {false, OldDir}                         ->
      {false, lists:append([Head, "/", OldDir])}
  end.

%%
%% Local Functions
%%


