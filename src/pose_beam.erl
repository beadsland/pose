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

%% @doc Beam binary utility functions used by {@link pose_code}.
%% @author Beads D. Land-Trujillo [http://twitter.com/beadsland]
%% @copyright 2012 Beads D. Land-Trujillo

-module(pose_beam).

%%
%% Include files
%%

%-define(debug, true).
-include_lib("pose/include/interface.hrl").

%%
%% Exported Functions
%%
-export([get_chunk/2, get_attribute/2, get_compiler_vsn/0, get_compiler_vsn/1, 
         get_binary_detail/2, slurp_binary/1]).

%%
%% API Functions
%%

-type beam() :: beam_lib:beam().
-type chunkref() :: beam_lib:chunkref().
-type chunk_error() :: {error, {beam_lib, beam_lib:chnk_rsn()}}.
-type chunk_result() ::  {ok, any()} | chunk_error().
-spec get_chunk(Beam :: beam(), ChunkRef :: chunkref()) -> chunk_result().
%% @doc Get data for a chunk of a beam. 
get_chunk(Beam, ChunkRef) ->
  case beam_lib:chunks(Beam, [ChunkRef], [allow_missing_chunks]) of
    {error, beam_lib, Reason}                    -> {error, {beam_lib, Reason}};
    {ok, {_Module, [{ChunkRef, missing_chunk}]}} -> {info, missing_chunk};
    {ok, {_Module, [{ChunkRef, Data}]}}          -> {ok, Data}
  end.

-type attribute_values() :: [term()] | undefined.
-type attribute_result() :: {ok, attribute_values()} | chunk_error().
-spec get_attribute(Beam :: beam(), Attribute :: atom()) -> attribute_result().
%% @doc Get attribute entry of a beam.
get_attribute(Beam, Attribute) ->
  case get_chunk(Beam, attributes) of
    {error, Reason}         -> {error, Reason};
    {info, missing_chunk}   -> {ok, undefined};
    {ok, Data}              -> {ok, proplists:get_value(Attribute, Data)}
  end.

-type compiler_vsn() :: string() | undefined.
-spec get_compiler_vsn() -> {ok, compiler_vsn()}.
%% @doc Get version of compiler currently loaded.
get_compiler_vsn() ->
  Info = beam_asm:module_info(compile),
  Options = proplists:get_value(options, Info),
  {ok, live_compiler_vsn(Options)}.

live_compiler_vsn([]) -> undefined;
live_compiler_vsn([{d, 'COMPILER_VSN', Version} | _Tail]) -> Version;
live_compiler_vsn([_Head | Tail]) -> live_compiler_vsn(Tail).

-type compiler_result() :: {ok, compiler_vsn()} | chunk_error().
-spec get_compiler_vsn(Beam :: beam()) -> compiler_result().
%% @doc Get version of compiler used to create beam.
get_compiler_vsn(Beam) ->
  case get_chunk(Beam, compile_info) of
    {error, Reason}       -> {error, Reason};
    {info, missing_chunk} -> {ok, undefined};
    {ok, Data}            -> {ok, proplists:get_value(version, Data)}
  end.
    
-type attribute() :: atom().
-type beam_lib_error() :: {beam_lib, term()}.
-type binary_detail_error() :: beam_lib_error() | {missing_chunk, attribute()}.
-type version() :: term().
-type package() :: term().
-spec get_binary_detail(Module :: module(), Binary ::  binary()) ->
          {ok, version(), package()} | {error, binary_detail_error()}.
%% @doc Get version and package of binary
%% @deprecated
get_binary_detail(Module, Binary) ->
    case beam_lib:version(Binary) of
        {error, beam_lib, What} -> {error, {beam_lib, What}};
        {ok, {Module, Version}} -> get_binary_detail(Module, Binary, Version)
    end.

-type posix() :: atom().
-type file_error_reason() :: posix() | badarg | terminated | system_limit.
-type slurp_error() :: {read, file_error_reason()} | beam_lib_error()
                        | no_module.
-type filename() :: file:filename().
-spec slurp_binary(Filename :: filename()) -> {ok, module(), binary()}
                                                | {error, slurp_error()}.
%% @doc Read binary file into memory.
%% @deprecated
slurp_binary(Filename) ->
    case file:read_file(Filename) of
        {ok, Binary}    -> slurp_binary(Filename, Binary);
        {error, What}   -> {error, {read, What}}
    end.

%%
%% Local Functions
%%

%%%
% Get binary detail
%%%

% Get package attribute of binary
get_binary_detail(Module, Binary, Version) ->
    case get_attribute(Binary, package) of
        {error, What}   -> {error, {read_beam, What}};
        {ok, [Package]} -> {ok, Version, Package};
        {ok, undefined} -> get_binary_detail(Module, Binary, Version, noattr)
    end.

% Figure out explicit package if no attribute found
get_binary_detail(Module, _Binary, Version, noattr) ->
    ModStr = atom_to_list(Module),
    case string:rstr(ModStr, ".") of
        0       -> {ok, Version, ''};
        Last    -> PackStr = string:substr(ModStr, 0, Last-1),
                   Package = list_to_atom(PackStr),
                   {ok, Version, Package}
    end.

%%%
% Slurp binary
%%%

% Extract meta information about binary.
slurp_binary(NewFile, Binary) ->
    case beam_lib:info(Binary) of
        {error, beam_lib, Reason}   ->
            {error, {beam_lib, Reason}};
        InfoList                    ->
            slurp_binary(NewFile, Binary, InfoList)
    end.

% Return binary with its fully qualified module name.
slurp_binary(_NewFile, Binary, Info) ->
    case lists:keyfind(module, 1, Info) of
        {module, Module}    -> {ok, Module, Binary};
        false               -> {error, nomodule}
    end.