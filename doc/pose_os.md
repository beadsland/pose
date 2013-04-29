

#Module pose_os#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


OS-related utility functions for use by [`pose_file`](pose_file.md).

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.4

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-shell_error">shell_error()</a>##



	shell_error() = {error, string() | <a href="#type-shell_error_tuple">shell_error_tuple()</a>}



###<a name="type-shell_error_term">shell_error_term()</a>##



	shell_error_term() = string() | integer() | atom()



###<a name="type-shell_error_tuple">shell_error_tuple()</a>##



	shell_error_tuple() = {<a href="#type-shell_error_term">shell_error_term()</a>, <a href="#type-shell_error_term">shell_error_term()</a> | <a href="#type-shell_error_tuple">shell_error_tuple()</a>}



###<a name="type-temp_file_error">temp_file_error()</a>##



	temp_file_error() = {error, {temp_dir, <a href="file.md#type-posix">file:posix()</a>}}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_temp_dir-0">get_temp_dir/0</a></td><td>Get system temporary directory.</td></tr><tr><td valign="top"><a href="#get_temp_file-0">get_temp_file/0</a></td><td>Get a unique name for a temporary file in the system temporary
directory.</td></tr><tr><td valign="top"><a href="#get_temp_file-1">get_temp_file/1</a></td><td>Get a unique name for a temporary file in the specified directory.</td></tr><tr><td valign="top"><a href="#shell_exec-1">shell_exec/1</a></td><td>Execute a command in operating system shell, capturing stdout and
stderr independently.</td></tr><tr><td valign="top"><a href="#shell_loop-3">shell_loop/3</a></td><td></td></tr><tr><td valign="top"><a href="#shell_loop-4">shell_loop/4</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="get_temp_dir-0"></a>

###get_temp_dir/0##


	get_temp_dir() -> {ok, <a href="file.md#type-filename">file:filename()</a>} | {error, <a href="file.md#type-posix">file:posix()</a>}
<br></br>


Get system temporary directory.<a name="get_temp_file-0"></a>

###get_temp_file/0##


	get_temp_file() -> {ok, <a href="file.md#type-filename">file:filename()</a>} | <a href="#type-temp_file_error">temp_file_error()</a>
<br></br>


Get a unique name for a temporary file in the system temporary
directory.<a name="get_temp_file-1"></a>

###get_temp_file/1##


	get_temp_file(Dir::<a href="file.md#type-filename">file:filename()</a>) -> {ok, <a href="file.md#type-filename_all">file:filename_all()</a>}
<br></br>


Get a unique name for a temporary file in the specified directory.<a name="shell_exec-1"></a>

###shell_exec/1##


	shell_exec(Command::string()) -> {ok, string()} | <a href="#type-shell_error">shell_error()</a>
<br></br>


Execute a command in operating system shell, capturing stdout and
stderr independently.<a name="shell_loop-3"></a>

###shell_loop/3##


`shell_loop(Port, Temp, Errors) -> any()`

<a name="shell_loop-4"></a>

###shell_loop/4##


`shell_loop(Port, Temp, ReadPid, Output) -> any()`

