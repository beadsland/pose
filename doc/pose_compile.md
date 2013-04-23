

#Module pose_compile#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Module compiler for [pose_code](#pose_code).

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.7

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



	command() = <a href="pose.md#type-command">pose:command()</a>



###<a name="type-compile_err">compile_err()</a>##



	compile_err() = {compile, {<a href="#type-compile_error_list">compile_error_list()</a>, <a href="#type-compile_warning_list">compile_warning_list()</a>}}



###<a name="type-compile_error_list">compile_error_list()</a>##



	compile_error_list() = [any()]



###<a name="type-compile_warning_list">compile_warning_list()</a>##



	compile_warning_list() = [any()]



###<a name="type-directory">directory()</a>##



	directory() = <a href="pose_file.md#type-filename">pose_file:filename()</a>



###<a name="type-ensure_err">ensure_err()</a>##



	ensure_err() = <a href="#type-file_err">file_err()</a> | <a href="#type-compile_err">compile_err()</a>



###<a name="type-ensure_rtn">ensure_rtn()</a>##



	ensure_rtn() = {ok, module(), binary()} | {ok, <a href="file.md#type-filename">file:filename()</a>} | {info, <a href="#type-info_type">info_type()</a>} | {error, <a href="#type-ensure_err">ensure_err()</a>}



###<a name="type-file_err">file_err()</a>##



	file_err() = {file, <a href="pose_file.md#type-file_err">pose_file:file_err()</a>}



###<a name="type-info_type">info_type()</a>##



	info_type() = readonly_dir | readonly | nobin | nosrc
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#ensure_compiled-2">ensure_compiled/2</a></td><td>Equivalent to <a href="#ensure_compiled-3"><tt>ensure_compiled(Command, Dir, false)</tt></a>.</td></tr><tr><td valign="top"><a href="#ensure_compiled-3">ensure_compiled/3</a></td><td>Confirm the most recently compiled binary for a command is
available in the current directory, compiling same if necessary.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="ensure_compiled-2"></a>

###ensure_compiled/2##


	ensure_compiled(Command::<a href="#type-command">command()</a>, Dir::<a href="#type-directory">directory()</a>) -> <a href="#type-ensure_rtn">ensure_rtn()</a>
<br></br>


Equivalent to [`ensure_compiled(Command, Dir, false)`](#ensure_compiled-3).<a name="ensure_compiled-3"></a>

###ensure_compiled/3##


	ensure_compiled(Command::<a href="#type-command">command()</a>, Dir::<a href="#type-directory">directory()</a>, Force::boolean()) -> <a href="#type-ensure_rtn">ensure_rtn()</a>
<br></br>


Confirm the most recently compiled binary for a command is
available in the current directory, compiling same if necessary.
If `Force` is true, binary will be recompiled if it can be.