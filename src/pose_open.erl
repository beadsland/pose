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

%% @doc A `pose'-command interface for reading and writing files in 
%% local filesystem.  Return the Pid of the process that maintains
%% the active file handle.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2013 Beads D. Land-Trujillo
 
%% @version 0.0.4

-module(pose_open).

-version("0.0.4").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

-import(gen_command).
-import(file).
-import(lists).
-import(io).

%%
%% Exported Functions
%%

% API entry points
-export([read/1, read/2, write/1, write/2, open/2]).

% private exports
-export([loop/3, run/3]).

%%
%% API Functions
%%

-type filename() :: file:name_all() | iodata().
-type modes() :: [file:mode() | ram | dos].
-type file_pid() :: pid().

-spec read(File :: filename()) -> file_pid().
read(File) -> open(File, [read]).

-spec read(File :: filename(), Modes :: modes()) -> file_pid().
read(File, Modes) -> open(File, [read | Modes]).

-spec write(File :: filename()) -> file_pid().
write(File) -> open(File, [write]).

-spec write(File :: filename(), Modes :: modes()) -> file_pid().
write(File, Modes) -> open(File, [write | Modes]).

-spec open(File :: filename(), Modes :: modes()) -> file_pid().
open(File, Modes) -> spawn_link(?MODULE, run, [?IO(self()), File, Modes]).

%%
%% Local Functions
%%

%@private callback function.
run(IO, File, Modes)  ->
  IsRead = lists:member(read, Modes),
  IsWrite = lists:member(write, Modes),
  IsDos = lists:member(dos, Modes),
  case file:open(File, lists:delete(dos, Modes)) of
    {error, Reason} -> exit({error, {File, Reason}});
    {ok, Device}	-> do_run(IO, File, Device, {IsRead, IsWrite, IsDos})
  end.

do_run(IO, File, Device, Access) ->
  case ?MODULE:loop(IO, Device, Access) of
    {error, Reason} -> exit({error, {File, Reason}});
    ok              -> exit(ok)
  end.

%@private Export to allow for hotswap.
loop(IO, Device, Access) ->
  {R, W, _D} = Access,
  receive
    {purging, _Pid, _Mod}									-> 
      ?MODULE:loop(IO, Device, Access);		% chase your tail
    {'EXIT', ExitPid, Reason}								->
      do_exit(IO, Device, Access, ExitPid, Reason);
    {stdin, Stdout, captln} when R, Stdout == IO#std.out	->
      do_readln(IO, Device, Access);
    {stdout, Stdin, Line} when W, Stdin == IO#std.in		->
      do_writeln(IO, Device, Access, Line);
    Noise													->
      do_noise(IO, Device, Access, Noise)
  end.

do_readln(IO, Device, Access) ->
  case io:get_line(Device, "") of
    eof		-> file:close(Device), ok;
    Line	-> ?STDOUT(Line), ?MODULE:loop(IO, Device, Access)
  end.

do_writeln(IO, Device, {R, W, true}, Line) ->
  Opts = [global, {return, list}],
  Output = re:replace(re:replace(Line, "\n\r", "\n", Opts), "\n", "\n\r", Opts),
  io:format(Device, "~s", [Output]),
  ?MODULE:loop(IO, Device, {R, W, true});
do_writeln(IO, Device, Access, Line) ->
  io:format(Device, "~s", [Line]), 
  ?MODULE:loop(IO, Device, Access).

do_exit(IO, Device, {R, W, D}, ExitPid, Reason) ->
  case ExitPid of
    Stdout when R, Stdout == IO#std.out	-> ?DEBUG("reader: ~p~n", [Reason]), ok;
    Stdin when W, Stdin == IO#std.in	-> ?DEBUG("writer: ~p~n", [Reason]), ok;
    _ 									-> ?MODULE:loop(IO, Device, {R, W, D})
  end.

do_noise(IO, Device, Access, Noise) ->
  ?STDERR("noise: ~p~n", [Noise]),
  ?MODULE:loop(IO, Device, Access).