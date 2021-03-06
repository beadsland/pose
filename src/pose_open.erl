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
 
%% @version 0.0.5

-module(pose_open).

-version("0.0.5").

%%
%% Include files
%%

-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%

% API entry points
-export([reader/1, reader/2, writer/1, writer/2, file/2]).

% private exports
-export([loop/3, run/3]).

%%
%% API Functions
%%

-type filename() :: file:name_all() | iodata().
-type modes() :: [file:mode() | ram | dos].
-type file_pid() :: pid().

-spec reader(File :: filename()) -> file_pid().
reader(File) -> file(File, [read]).

-spec reader(File :: filename(), Modes :: modes()) -> file_pid().
reader(File, Modes) -> file(File, [read | Modes]).

-spec writer(File :: filename()) -> file_pid().
writer(File) -> file(File, [write]).

-spec writer(File :: filename(), Modes :: modes()) -> file_pid().
writer(File, Modes) -> file(File, [write | Modes]).

-spec file(File :: filename(), Modes :: modes()) -> file_pid().
file(File, Modes) -> spawn_link(?MODULE, run, [?IO(self()), File, Modes]).

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
    {stdout, Stdin, eof} when W, Stdin == IO#std.in         ->
      do_close(Device, Access);      
    {stdout, Stdin, Line} when W, Stdin == IO#std.in		->
      do_writeln(IO, Device, Access, Line);
    Noise													->
      ?DONOISE, ?MODULE:loop(IO, Device, Access)
  end.

% Read a line of text from file (triggered by captln).
do_readln(IO, Device, Access) ->
  case io:get_line(Device, "") of
    eof		-> ?STDOUT(eof), do_close(Device, Access);
    Line	-> ?STDOUT(Line), ?MODULE:loop(IO, Device, Access)
  end.

% Write a line of text to file, fixing eol if necessary.
do_writeln(IO, Device, {R, W, D}, Line) when D ->
  do_writeln(IO, Device, {R, W, D}, unix_to_dos(Line), dowrite);
do_writeln(IO, Device, Access, Line) -> 
  do_writeln(IO, Device, Access, Line, dowrite).

do_writeln(IO, Device, Access, Line, dowrite) ->
  case file:write(Device, Line) of
    {error, Reason} -> {error, {writer, Reason}};
    ok              -> ?MODULE:loop(IO, Device, Access)
  end.

% Swap unix single-character eol for dos double-character eol.
unix_to_dos(Line) -> unix_to_dos(lists:flatten(Line), []).

unix_to_dos([], Part) -> lists:reverse(Part);
unix_to_dos([$\r | [$\n | Rest]], Part) -> unix_to_dos(Rest, ["\r\n" | Part]);
unix_to_dos([$\n | Rest], Part) -> unix_to_dos(Rest, ["\r\n" | Part]);
unix_to_dos([Next | Rest], Part) -> unix_to_dos(Rest, [Next | Part]).

% Close file, reporting any delayed write errors and/or errors in file close.
do_close(Device, {_R, W, _D}) ->
  case file:close(Device) of
    {error, enospc} when W  -> do_close(Device, enospc);
    {error, Reason}         -> {error, {close, Reason}};
    ok                      -> ok
  end;
do_close(Device, enospc) ->
  case file:close(Device) of
    {error, Reason} -> {error, {writer_close, {Reason, enospc}}};
    ok              -> {error, {writer, enospc}}
  end.

% Handle exit messages, closing file if connected stdin or stdout process exits.
do_exit(IO, Device, Access, ExitPid, Reason) ->
  {R, W, _D} = Access, 
  case ExitPid of
    Stdout when R, Stdout == IO#std.out	-> ?DEBUG("reader: ~p~n", [Reason]),
                                           do_close(Device, Access);
    Stdin when W, Stdin == IO#std.in	-> ?DEBUG("writer: ~p~n", [Reason]),
                                           do_close(Device, Access);
    _ 									-> ?MODULE:loop(IO, Device, Access)
  end.