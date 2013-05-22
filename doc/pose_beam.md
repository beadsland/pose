

#Module pose_beam#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Beam binary utility functions used by [`pose_code`](pose_code.md).

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.4

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-attribute_result">attribute_result()</a>##



	attribute_result() = {ok, <a href="#type-attribute_values">attribute_values()</a>} | <a href="#type-chunk_error">chunk_error()</a>



###<a name="type-attribute_values">attribute_values()</a>##



	attribute_values() = [term()] | undefined



###<a name="type-beam">beam()</a>##



	beam() = <a href="beam_lib.md#type-beam">beam_lib:beam()</a>



###<a name="type-chunk_error">chunk_error()</a>##



	chunk_error() = {error, {beam_lib, <a href="beam_lib.md#type-chnk_rsn">beam_lib:chnk_rsn()</a>}}



###<a name="type-chunk_result">chunk_result()</a>##



	chunk_result() = {ok, any()} | <a href="#type-chunk_error">chunk_error()</a>



###<a name="type-chunkref">chunkref()</a>##



	chunkref() = <a href="beam_lib.md#type-chunkref">beam_lib:chunkref()</a>



###<a name="type-compiler_result">compiler_result()</a>##



	compiler_result() = {ok, <a href="#type-compiler_vsn">compiler_vsn()</a>} | <a href="#type-chunk_error">chunk_error()</a>



###<a name="type-compiler_vsn">compiler_vsn()</a>##



	compiler_vsn() = atom() | undefined



###<a name="type-file_error_reason">file_error_reason()</a>##



	file_error_reason() = <a href="#type-posix">posix()</a> | badarg | terminated | system_limit



###<a name="type-filename">filename()</a>##



	filename() = <a href="file.md#type-filename">file:filename()</a>



###<a name="type-module_version">module_version()</a>##



	module_version() = term() | [term()]



###<a name="type-package">package()</a>##



	package() = atom() | ''



###<a name="type-posix">posix()</a>##



	posix() = atom()



###<a name="type-slurp_error">slurp_error()</a>##



	slurp_error() = {read, <a href="#type-file_error_reason">file_error_reason()</a>} | no_module



###<a name="type-source">source()</a>##



	source() = <a href="file.md#type-filename">file:filename()</a> | undefined
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_attribute-2">get_attribute/2</a></td><td>Get attribute entry of a beam.</td></tr><tr><td valign="top"><a href="#get_chunk-2">get_chunk/2</a></td><td>Get data for a chunk of a beam.</td></tr><tr><td valign="top"><a href="#get_compiler_vsn-0">get_compiler_vsn/0</a></td><td>Get version of compiler currently loaded.</td></tr><tr><td valign="top"><a href="#get_compiler_vsn-1">get_compiler_vsn/1</a></td><td>Get version of compiler used to create beam.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Get module name of a beam.</td></tr><tr><td valign="top"><a href="#get_module_vsn-1">get_module_vsn/1</a></td><td>Get version of a beam.</td></tr><tr><td valign="top"><a href="#get_package-1">get_package/1</a></td><td>Get package of a beam.</td></tr><tr><td valign="top"><a href="#get_source-1">get_source/1</a></td><td>Get .erl file source of a beam.</td></tr><tr><td valign="top"><a href="#slurp_binary-1">slurp_binary/1</a></td><td>Read binary file into memory.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_attribute-2"></a>

###get_attribute/2##


	get_attribute(Beam::<a href="#type-beam">beam()</a>, Attribute::atom()) -> <a href="#type-attribute_result">attribute_result()</a>
<br></br>


Get attribute entry of a beam.<a name="get_chunk-2"></a>

###get_chunk/2##


	get_chunk(Beam::<a href="#type-beam">beam()</a>, ChunkRef::<a href="#type-chunkref">chunkref()</a>) -> <a href="#type-chunk_result">chunk_result()</a>
<br></br>


Get data for a chunk of a beam.<a name="get_compiler_vsn-0"></a>

###get_compiler_vsn/0##


	get_compiler_vsn() -> {ok, <a href="#type-compiler_vsn">compiler_vsn()</a>}
<br></br>


Get version of compiler currently loaded.<a name="get_compiler_vsn-1"></a>

###get_compiler_vsn/1##


	get_compiler_vsn(Beam::<a href="#type-beam">beam()</a>) -> <a href="#type-compiler_result">compiler_result()</a>
<br></br>


Get version of compiler used to create beam.<a name="get_module-1"></a>

###get_module/1##


	get_module(Beam::<a href="#type-beam">beam()</a>) -> {ok, module()} | <a href="#type-chunk_error">chunk_error()</a>
<br></br>


Get module name of a beam.<a name="get_module_vsn-1"></a>

###get_module_vsn/1##


	get_module_vsn(Beam::<a href="#type-beam">beam()</a>) -> {ok, <a href="#type-module_version">module_version()</a>} | <a href="#type-chunk_error">chunk_error()</a>
<br></br>


Get version of a beam.<a name="get_package-1"></a>

###get_package/1##


	get_package(Beam::<a href="#type-beam">beam()</a>) -> {ok, <a href="#type-package">package()</a>} | <a href="#type-chunk_error">chunk_error()</a>
<br></br>


Get package of a beam.<a name="get_source-1"></a>

###get_source/1##


	get_source(Beam::<a href="#type-beam">beam()</a>) -> {ok, <a href="#type-source">source()</a>} | <a href="#type-chunk_error">chunk_error()</a>
<br></br>


Get .erl file source of a beam.<a name="slurp_binary-1"></a>

###slurp_binary/1##


	slurp_binary(Filename::<a href="#type-filename">filename()</a>) -> {ok, module(), binary()} | {error, <a href="#type-slurp_error">slurp_error()</a>}
<br></br>


Read binary file into memory.