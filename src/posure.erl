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

%% @doc Posure package import checker for pose.  Checks pose-compatible
%% command modules for unimported library modules.
%% @end
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012, 2013 Beads D. Land-Trujillo

%% @version 0.1.7

-define(module, posure).

% BEGIN POSE PACKAGE PATTERN
-ifndef(package).
-module(?module).
-package(default).
-else.
-module(?package.?module).
-package(?package).
-endif.
% END POSE PACKAGE PATTERN

-version("0.1.7").

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").
-include_lib("pose/include/macro.hrl").

% BEGIN POSE PACKAGE IMPORTS
-ifdef(package).
-import(gen_command).
-import(filelib).
-import(filename).
-import(file).
-import(lists).
-import(re).
-import(sets).
-import(proplists).
-import(ordsets).
-import(string).
-endif.
% END POSE PACKAGE IMPORTS

%%
%% Exported Functions
%%

-behaviour(gen_command).

% API entry points
-export([start/0, start/1, run/3]).

% Private callbacks
-export([do_run/2]).

%%
%% API Functions
%%

-spec start() -> no_return().
%% @equiv start([])
start() -> start([]).

-spec start(Param :: [atom()]) -> no_return().
%% @doc Start as a blocking function.
start(Param) -> gen_command:start(Param, ?MODULE).

-spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
%% doc Start as a `pose' command.
run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).

%%
%% Callback Functions
%%

%% @private Callback entry point for gen_command behaviour.
do_run(IO, ARG) ->
  ?STDOUT("Running Posure ~s package import checker~n", [?VERSION(?MODULE)]),
  Src = filename:absname("src"),
  Pattern = lists:append(Src, "/*.erl"),
  Source = filelib:wildcard(Pattern),
  case slurp_pose_sources(Source) of
    {error, What} -> exit({ARG#arg.cmd, {slurp, What}});
    {ok, Slurps}  -> case send_warnings(IO, Slurps) of
                       sure     -> ?STDOUT("Quite sure!\n"),
                                   exit(ok);
                       notsure  -> ?STDOUT("Not all that sure.\n"),
                                   exit(notsure)
                     end
  end.

%%
%% Local Functions
%%

%%%
% Send warnings to stdout
%%%

% Get names of all modules slurped.
send_warnings(IO, Slurps) ->
  Keys = proplists:get_keys(Slurps),
  ?DEBUG("slurps: ~p~n", [Keys]),
  Commands = [get_command_name(X) || X <- Keys],
  ?DEBUG("pose commands: ~p~n", [Commands]),
  send_warnings(IO, Commands, Slurps).

% Confirm that all imports are conditonal to use of package option.
send_warnings(_IO, _Commands, []) -> sure;
send_warnings(IO, Commands, [{File, Data} | Tail]) ->
  Command = get_command_name(File),
  {ok, Cond, Uncond} = get_imported_modules(IO, Command, Data),
  send_warnings(IO, Commands, File, Data, Tail, {Cond, Uncond}).

% For each file, identify where imports and calls don't match up.
send_warnings(IO, Commands, File, Data, Tail, {Cond, Uncond}) -> 
  ThisCommand = get_command_name(File),
  Imports = lists:append(Cond, Uncond),
  ?DEBUG("imports: ~p~n", [Imports]),
  Called = get_called_modules(Data),
  ?DEBUG("called: ~p~n", [Called]),

  Unimported = test_unimported(IO, ThisCommand, Imports, Called, Commands),
  BadDirect = test_baddirect(IO, ThisCommand, Called, Commands),
  UnqualLoops = test_unqualloops(IO, ThisCommand, Data),
  Noncalled = test_noncalled(IO, ThisCommand, Imports, Called), % warn only

  UncondCalled = lists:subtract(Uncond, Noncalled),
  
  case length(UncondCalled ++ Unimported ++ BadDirect ++ UnqualLoops) of
    0       -> send_warnings(IO, Commands, Tail);
    _Else   -> notsure
  end.

% Identify and warn about called modules that haven't been imported.
test_unimported(IO, ThisCommand, Imports, Called, Commands) ->
  Unimported = lists:subtract(lists:subtract(Called, Imports), Commands),
  [send_unimported_warning(IO, ThisCommand, X) || X <- Unimported],
  Unimported.

send_unimported_warning(IO, Command, Module) ->
  ?STDOUT("~s: packaged call to unimported module '~s'~n", [Command, Module]).

% Identify and warn about imported modules that are actually pose commands.
test_baddirect(IO, ThisCommand, Called, Commands) ->
  BadDirect = [X || X <- Called, lists:member(X, Commands),
                    is_submodule(X, ThisCommand) == false],
  [send_baddirect_warning(IO, ThisCommand, X) || X <- BadDirect],
  BadDirect.

send_baddirect_warning(IO, Command, Module) ->
  ?STDOUT("~s: calls pose command module '~s'~n", [Command, Module]).

% Identify calls to loop functions that are not fully qualified.
test_unqualloops(IO, ThisCommand, Data) ->
  Pattern = "^(.*[^a-zA-Z0-9_@])([a-zA-Z0-9_@]*loop)\\(",
  {ok, MP} = re:compile(Pattern, [multiline]),
    case re:run(Data, MP, [global, {capture, [1,2], list}]) of
    nomatch			->
      [];
    {match, Loops}	->
      Tests = [test_unqualloop_matches(X) || X <- Loops],
      Set = ordsets:from_list(Tests),
      Unquals = ordsets:to_list(ordsets:del_element(nomatch, Set)),
      [send_unqualloop_warning(IO, ThisCommand, X) || X <- Unquals],
      Unquals
  end.

test_unqualloop_matches(["", _Loop]) -> nomatch;
test_unqualloop_matches(["-spec ", _Loop]) -> nomatch;
test_unqualloop_matches([Prefix, Loop]) ->
  case lists:last(Prefix) of $: -> nomatch; $\n -> nomatch; _ -> Loop end.

send_unqualloop_warning(IO, Command, Module) ->
  ?STDOUT("~s: unqualified call to '~s'~n", [Command, Module]).

% Identify and warn about imported modules that haven't been called.
test_noncalled(IO, ThisCommand, Imports, Called) ->
  Noncalled = lists:subtract(Imports, Called),
  [send_noncalled_warning(IO, ThisCommand, X) || X <- Noncalled],
  Noncalled.

send_noncalled_warning(IO, Command, Module) ->
  ?STDOUT("~s: imports unused module '~s'~n", [Command, Module]).

%%%
% Analysis utility functions
%%%

% Is either of these two modules a submodule of the other?
is_submodule(X, Y) ->
  TestX1 = lists:prefix(X ++ "_", Y),
  TestY1 = lists:prefix(Y ++ "_", X),
  {ok, MP} = re:compile("^([^_]+)_"),
  if TestX1; TestY1     ->
       true;
     true               ->
       TestX2 = re:run(X, MP, [{capture, [1], list}]),
       TestY2 = re:run(X, MP, [{capture, [1], list}]),
       is_submodule(X, Y, TestX2, TestY2)
  end.

% Are both submodules of the same command?
is_submodule(_X, _Y, nomatch, _) -> false;
is_submodule(_X, _Y, _, nomatch) -> false;
is_submodule(_X, _Y, {match, XPre}, {match, YPre}) ->
  if XPre == YPre   -> true;
     true           -> false
  end.

% Extract the module name from a full source file path.
get_command_name(File) ->
  {ok, MP} = re:compile("\\/([^\\/]+)\\.erl$"),
  case re:run(File, MP, [{capture, [1], list}]) of
    nomatch                 -> File;
    {match, [Command]}      -> Command
  end.

% Scan a file for package conditional import/1 directives.
get_imported_modules(IO, Command, Data) when is_binary(Data) ->
  get_imported_modules(IO, Command, binary_to_list(Data)); 
get_imported_modules(IO, Command, Data) -> 
  get_imported_modules(IO, Command, string:tokens(Data, "\n"), endif).

% Extract module from matched import/1 directive.
get_imported_modules(_IO, _Command, [], _Cond) -> {ok, [], []};
get_imported_modules(IO, Cmd, [Head | Tail]=Lines, Cond) ->  
  {ok, MP} = re:compile("^-import\\(([^),]*)\\)\\."),  
  case re:run(Head, MP, [{capture, [1], list}]) of
    nomatch           -> get_imported_modules(IO, Cmd, Lines, Cond, not_import);
    {match, [Import]} -> get_imported_modules(IO, Cmd, Tail, Cond, Import)
  end.

% Confirm each import found is within an ifdef(package) conditional.
get_imported_modules(IO, Command, [Head | Tail], endif, not_import) ->
  case re:run(Head, "^-ifdef\\(package\\)\\.", [{capture, none}]) of
    nomatch -> get_imported_modules(IO, Command, Tail, endif);
    match   -> get_imported_modules(IO, Command, Tail, ifdef)
  end;
get_imported_modules(IO, Command, [Head | Tail], ifdef, not_import) ->
  case re:run(Head, "^-endif\\.", [{capture, none}]) of
    nomatch -> get_imported_modules(IO, Command, Tail, ifdef);
    match   -> get_imported_modules(IO, Command, Tail, endif)
  end;
get_imported_modules(IO, Command, Tail, endif, Import) when is_list(Import) ->
  Warning = "import/1 without package conditional",
  ?STDOUT("~s: ~s: '~s'~n", [Command, Warning, Import]),
  {ok, Cond, Uncond} = get_imported_modules(IO, Command, Tail, endif),
  {ok, Cond, [Import | Uncond]};
get_imported_modules(IO, Command, Tail, ifdef, Import) when is_list(Import) ->
  {ok, Cond, Uncond} = get_imported_modules(IO, Command, Tail, ifdef),
  {ok, [Import | Cond], Uncond}.

% Scan a file for all fully qualified module calls.
get_called_modules(Data) ->
  {ok, MP} = re:compile("^[^\\%\\n]*[\\s\\[\\{\\(\\,]([a-z_]+)\\:[a-z_]+\\(",
                        [multiline]),
  case re:run(Data, MP, [global, {capture, [1], list}]) of
    nomatch             -> [];
    {match, Imports}    -> List = [lists:nth(1, X) || X <- Imports],
                           sets:to_list(sets:from_list(List))
  end.

%%%
% Slurp pose sources
%%%

% Read the contents of each file into memory.
slurp_pose_sources([]) -> {ok, []};
slurp_pose_sources([Head | Tail]) ->
  case file:read_file(Head) of
    {error, What}   -> {error, {What, Head}};
    {ok, Data}      -> slurp_pose_sources(Tail, {Head, Data})
  end.

% Discard files that aren't actually pose-compatible commands.
slurp_pose_sources(Tail, {Head, Data}) ->
  {ok, MP} = re:compile("^-package\\(default\\)\\.", [multiline]),
  case re:run(Data, MP, [{capture, none}]) of
    nomatch -> slurp_pose_sources(Tail);
    match   -> case slurp_pose_sources(Tail) of
                 {error, What}  -> {error, What};
                 {ok, Slurps}   -> {ok, [{Head, Data} | Slurps]}
               end
  end.