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

%% @doc Watch for the creation of a text file, tail it until its associated 
%% lock file has been removed, and then exit.  Used by {@link pose_shell} to 
%% receive the `stdout' of a shell command.
%%
%% Note: Code is not Unicode safe. Assumes all shell commands produce `latin1'
%% output.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo

%% @version 0.0.1
-module(pose_shout).
-version("0.0.1").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API entry point

-export([monitor/1]).

% Private exports

-export([run/2, loop/2, loop/5]).

%%
%% API Functions
%%

-type filename() :: string().
-spec monitor(File :: filename()) -> pid().
%% @doc Watch a text file, copying each line written to it to calling process
%% as standard output, until an associated lock file is removed (this signaling 
%% that writing to the file has finished.)
%% @end
monitor(File) -> spawn_link(?MODULE, run, [?IO(self()), File]).

%%
%% Local Functions
%%

% @private callback function
run(IO, File) ->
  ENV = ?ENV, ?INIT_POSE,
  case loop(IO, File) of
    {error, Reason} -> exit({error, {filename:basename(File), Reason}});
    ok              -> exit(ok)
  end.

% @private exported for fully-qualified calls
loop(IO, File) -> 
  receive 
    {'EXIT', Stdout, _Reason} when Stdout == IO#std.in -> ok
  after 500 ->
    Exists = filelib:is_file(File),
    if Exists   -> do_open(IO, File);
       true     -> loop(IO, File)
    end
  end.

% File has been created, so start reading it.
do_open(IO, File) ->
  case file:open(File, [read]) of
    {error, Reason} -> {error, {open, Reason}};
    {ok, Handle}    -> loop(IO, File, Handle, [], 0)
  end.

% @private exported for fully-qualified calls
loop(IO, File, Handle, Chars, Size) ->
  receive
    {'EXIT', Stdout, _Reason} when Stdout == IO#std.in -> ok
  after 500 ->
    case read_chars(File, Handle, Size) of
      {error, Reason}       -> {error, {read, Reason}};
      ok                    -> do_test_lock(IO, File, Handle, Chars, Size);
      {ok, Data, NewSize}   -> NewChars = send_chars(IO, Chars, Data),
                               do_test_lock(IO, File, Handle, NewChars, NewSize)
    end
  end.

% Read however many characters have been added to the file.  Assumes `latin1'.
read_chars(File, Handle, Size) ->
  case pose_file:size(File) of
    {error, Reason} -> {error, {file_size, Reason}};
    {ok, nofile}    -> {error, truncated};
    {ok, NewSize}   -> read_chars(File, Handle, Size, NewSize)
  end.

read_chars(_File, _Handle, Size, NewSize) when NewSize == Size -> ok;
read_chars(File, _Handle, Size, NewSize) when NewSize < Size ->
  {error, truncated};
read_chars(File, Handle, Size, NewSize) ->
  case file:read(Handle, NewSize - Size) of
    {error, Reason} -> {error, {read, Reason}};
    eof             -> {error, truncated};
    {ok, Data}      -> read_chars(File, Handle, Size, NewSize, Data)
  end.

read_chars(File, _Handle, Size, NewSize, Data) ->
  Length = string:len(Data),
  SizeDiff = NewSize - Size,
  if Length /= SizeDiff -> {error, truncated};
     true               -> {ok, Data, NewSize}
  end.

% Send complete lines to stdout, buffering pending an eol.
send_chars(_IO, Chars, []) -> Chars;
send_chars(IO, Chars, [$\r | [$\n | Data]]) ->
  send_chars(IO, Chars, [$\n | Data]);
send_chars(IO, Chars, [$\n | Data]) ->
  ?STDOUT("~s~n", [lists:reverse(Chars)]), 
  send_chars(IO, [], Data);
send_chars(IO, Chars, [Next | Data]) -> send_chars(IO, [Next | Chars], Data).

% Send any buffered characters as a final line.
flush_chars(_IO, []) -> [];
flush_chars(IO, Chars) -> send_chars(IO, Chars, "\n").

% Test to see if associated lock file has been removed.
do_test_lock(IO, File, Handle, Chars, Size) ->
  Locked = filelib:is_file(io_lib:format("~s.lock", [File])),
  if not Locked -> flush_chars(IO, Chars), ?STDOUT(eof), ok;
     true       -> loop(IO, File, Handle, Chars, Size)
  end.