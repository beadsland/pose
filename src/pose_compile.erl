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

%% @doc Module compiler for {@section pose_code}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @version 0.1.8
-module(pose_compile).
-version("0.1.8").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

%%
%% Exported Functions
%%

-export([ensure_compiled/2, ensure_compiled/3]).

%%
%% API Functions
%%

-type command() :: pose:command().
-type directory() :: pose_file:filename().
-type info_type() :: readonly_dir | readonly | nobin | nosrc.
-type file_err() :: {file, pose_file:info_error_atom()}.
-type compile_error_list() :: [any()].
-type compile_warning_list() :: [any()].
-type compile_err() :: {compile,
                        {compile_error_list(), compile_warning_list()}}.
-type ensure_err() :: file_err() | compile_err().
-type ensure_rtn() :: {ok, module(), binary()} | {ok, file:filename()}
                        | {info, info_type()} | {error, ensure_err()}.
-spec ensure_compiled(Command :: command(), Dir :: directory()) ->
        ensure_rtn().
%% @equiv ensure_compiled(Command, Dir, false)
ensure_compiled(Command, Dir) -> ensure_compiled(Command, Dir, false).

-spec ensure_compiled(Command :: command(), Dir :: directory(),
                      Force :: boolean()) -> ensure_rtn().
%% @doc Confirm the most recently compiled binary for a command is
%% available in the current directory, compiling same if necessary.
%% If `Force' is true, binary will be recompiled if it can be.
%% @end
ensure_compiled(Cmd, Dir, Force) ->
  Filename = filename:join(Dir, string:concat(Cmd, ".beam")),
  case pose_file:can_write(Filename) of
    {error, What}   -> {error, {file, What}};
    false           -> ensure_binary(Cmd, Dir, readonly);
    true            -> ensure_compiled(Cmd, Dir, Force, writable)
  end.

%%
%% Local Functions
%%

%%%
% Ensure compiled
%%%

% Find any source file, and get modification date of same.
ensure_compiled(Cmd, BinDir, Force, writable) ->
  case parallel_src(BinDir, Cmd) of
    nosrc           -> ensure_binary(Cmd, BinDir, nosrc);
    {ok, SrcDir}    -> ensure_compiled(Cmd, BinDir, Force, SrcDir)
  end;
ensure_compiled(Cmd, BinDir, Force, SrcDir) ->
  Filename = filename:join(SrcDir, string:concat(Cmd, ".erl")),
  case pose_file:last_modified(Filename) of
    {error, What}   -> {error, {file, What}};
    SrcMod          -> ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod)
  end.

% If we can't compile from source file, confirm we can use binary we have.
ensure_binary(Cmd, Dir, Why) ->
  Filename = filename:join(Dir, string:concat(Cmd, ".beam")),
  HaveBinary = pose_file:can_read(Filename),
  if HaveBinary -> {info, Why};
     true       -> {info, nobin}  % i.e., search next dir in path
  end.

% Get modification date of binary file.
ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod) ->
  BinFile = filename:join(BinDir, string:concat(Cmd, ".beam")),
  case pose_file:last_modified(BinFile) of
    {error, What}   ->
      {error, {file, What}};
    BinMod          ->
      ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod, BinMod)
  end.

% Compare modification dates and compile if source is newer.
ensure_compiled(Cmd, BinDir, Force, SrcDir, SrcMod, BinMod) ->
  if SrcMod > BinMod; Force -> 
       do_compile(SrcDir, Cmd, BinDir);
     true                   -> 
       {ok, filename:join(BinDir, string:concat(Cmd, ".beam"))}
  end.

%%%
% Do compile
%%%

% Make sure we've got a directory to write to.
do_compile(SrcDir, Cmd, BinDir)  ->
  case file:make_dir(BinDir) of
    ok              -> do_compile(SrcDir, Cmd, BinDir, true_dir);
    {error, eexist} -> do_compile(SrcDir, Cmd, BinDir, true_dir);
    {error, What}   -> {error, {file, {What, BinDir}}}
  end.

% Get needed compiler options.
do_compile(SrcDir, Cmd, BinDir, true_dir) ->
  case get_otp_includes(BinDir) of
    {error, What}   -> {error, {includes, What}};
    {ok, InclList}  -> do_compile(SrcDir, Cmd, BinDir, InclList)
  end;
do_compile(SrcDir, Cmd, BinDir, InclList) ->
  Options = [verbose, warnings_as_errors, return_errors, binary,
            {outdir, BinDir}] ++ InclList,
  Release = erlang:system_info(otp_release),
  if Release > "R16" -> 
       do_compile(SrcDir, Cmd, BinDir, InclList, Options);
     true            -> 
       do_compile(SrcDir, Cmd, BinDir, InclList, {package, Options})
  end.

% Include package option for Erlang/OTP releases that support packages.
do_compile(SrcDir, Cmd, BinDir, InclList, {package, Options}) ->
  case get_otp_package(BinDir) of
    {error, What}   -> {error, {package, What}};
    {ok, Package}   -> NewOptions = [{d, package, Package} | Options],
                       do_compile(SrcDir, Cmd, BinDir, InclList, NewOptions)
  end;
do_compile(SrcDir, Cmd, BinDir, _InclList, Options) ->
  Filename = filename:join(SrcDir, string:concat(Cmd, ".erl")),
  Outfile = filename:join(BinDir, string:concat(Cmd, ".beam")),
  now_compile(Filename, Options, Outfile).

% Actually attempt to compile file.
now_compile(Filename, Options, Outfile) ->
  case compile:file(Filename, Options) of
    error                     -> 
      {error, {compile, unspecified_error}};
    {error, Errors, Warnings} -> 
      now_compile_error(Filename, Errors, Warnings);
    {ok, ModuleName, Binary}  ->
      now_compile(Filename, Options, Outfile, ModuleName, Binary)
  end.

% Write successfully compiled binary out to file system.
now_compile(_Filename, _Options, Outfile, ModuleName, Binary) ->
  case file:write_file(Outfile, Binary) of
    {error, What}   -> {error, {file, {What, Outfile}}};
    ok              -> {ok, ModuleName, Binary}
  end.

% Streamline error result to our nested tuple form.
now_compile_error(Filename, Errors, Warnings) -> 
  [First | _Rest] = lists:append(Errors, Warnings),
  {Filename, [ErrorInfo | _MoreErrorInfo]} = First,
  {Line, Module, ErrorDescriptor} = ErrorInfo,
  Where = io_lib:format("~s, line ~p", [filename:basename(Filename), Line]),
  What = Module:format_error(ErrorDescriptor),
  {error, {compile, {Where, What}}}.
  
%%%
% Get OTP compliant package
%%%

%% @todo make this chdir safe
get_otp_package(BinDir) ->
  Pwd = filename:absname(""),
  {ok, MP} = re:compile("^" ++ Pwd ++ "/(.*)$"),
  case re:run(BinDir, MP, [{capture, [1], list}]) of
    nomatch         -> {error, off_pwd};
    {match, [Path]} -> get_otp_package(BinDir, Path)
  end.

get_otp_package(BinDir, [First | Rest]) when First == $\. ->
  Path = re:replace([First | Rest], "^..", "__", [{return, list}, global]),
  ?DEBUG("package: .. as __\n"),
  get_otp_package(BinDir, Path);
get_otp_package(_BinDir, Path) ->
  Package = re:replace(Path, "\/", ".", [{return, list}, global]),
  ?DEBUG("package: ~s~n", [Package]),
  {ok, list_to_atom(Package)}.

%%%
% Get OTP standard include paths
%%%

get_otp_includes(BinDir) ->
  case init:get_argument(deps) of
    {ok, [[Value]]} -> Deps = Value;
    _               -> Deps = "deps"
  end,

  ?DEBUG("deps: ~p~n", [Deps]),

  case pose_file:find_parallel_folder("ebin", "_temp_", BinDir) of
    {true, TempDir} ->
      {ok, get_otp_includes(TempDir, ["apps"]) ++
           get_otp_includes(filename:absname("_temp_"), [Deps])};
    {false, BinDir} ->
      {error, not_otp}
  end.

get_otp_includes(_TempDir, []) -> [];
get_otp_includes(TempDir, [Head | Tail]) ->
  Include = re:replace(TempDir, "_temp_.*\$", Head, [{return, list}]),

  ?DEBUG("include: ~p~n", [[TempDir, Include, Head]]),

  case pose_file:can_read(Include) of
    true            -> [{i, Include} | get_otp_includes(TempDir, Tail)];
    false           -> get_otp_includes(TempDir, Tail);
    {error, What}   -> {error, {file, What}}
  end.

%%%
% Find parallel source directory
%%%

% Find candidate src directory parallel to ebin.
parallel_src(BinDir, Cmd) ->
  ?DEBUG("Seeking parallel src: ~s~n", [BinDir]),
  case pose_file:find_parallel_folder("ebin", "src", BinDir) of
    {true, SrcPath} -> parallel_src(BinDir, Cmd, SrcPath);
    _Else           -> ?DEBUG("Didn't find parallel src\n"), nosrc
  end.

% Confirm it's readable and return result.
parallel_src(_BinDir, Command, SrcDir) ->
  Filename = filename:join(SrcDir, string:concat(Command, ".erl")),
  case pose_file:can_read(Filename) of
    true    -> {ok, SrcDir};
    false   -> nosrc
  end.