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

-export([get_temp_file/0, get_temp_file/1, get_temp_dir/0]). 

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