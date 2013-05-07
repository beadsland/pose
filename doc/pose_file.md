

#Module pose_file#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


File-related utility functions for use by [`pose_code`](pose_code.md).

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.11

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-datestamp_return">datestamp_return()</a>##



	datestamp_return() = {ok, <a href="#type-datestamp_value">datestamp_value()</a>} | <a href="#type-file_info_error">file_info_error()</a>



###<a name="type-datestamp_value">datestamp_value()</a>##



	datestamp_value() = <a href="calendar.md#type-date_time">calendar:date_time()</a> | nofile



###<a name="type-exit_status">exit_status()</a>##



	exit_status() = integer()



###<a name="type-file_info_error">file_info_error()</a>##



	file_info_error() = {error, <a href="#type-file_info_reason">file_info_reason()</a>}



###<a name="type-file_info_reason">file_info_reason()</a>##



	file_info_reason() = {<a href="file.md#type-name_all">file:name_all()</a>, <a href="#type-info_error_atom">info_error_atom()</a>}



###<a name="type-folder">folder()</a>##



	folder() = nonempty_string()



###<a name="type-info_error_atom">info_error_atom()</a>##



	info_error_atom() = <a href="file.md#type-posix">file:posix()</a> | badarg



###<a name="type-parallel_result">parallel_result()</a>##



	parallel_result() = {false, <a href="#type-path_string">path_string()</a>} | {true, <a href="#type-path_string">path_string()</a>}



###<a name="type-path">path()</a>##



	path() = <a href="#type-path_string">path_string()</a> | <a href="#type-path_list">path_list()</a>



###<a name="type-path_list">path_list()</a>##



	path_list() = {folders, [<a href="#type-folder">folder()</a>]}



###<a name="type-path_string">path_string()</a>##



	path_string() = nonempty_string()



###<a name="type-permissions_return">permissions_return()</a>##



	permissions_return() = boolean() | <a href="#type-file_info_error">file_info_error()</a>



###<a name="type-realname_error">realname_error()</a>##



	realname_error() = {error, {<a href="#type-exit_status">exit_status()</a>, [string()]}}



###<a name="type-realname_result">realname_result()</a>##



	realname_result() = <a href="#type-path_string">path_string()</a> | <a href="#type-realname_error">realname_error()</a>



###<a name="type-size_return">size_return()</a>##



	size_return() = {ok, <a href="#type-size_value">size_value()</a>} | <a href="#type-file_info_error">file_info_error()</a>



###<a name="type-size_value">size_value()</a>##



	size_value() = integer() | nofile



###<a name="type-temp_file_error">temp_file_error()</a>##



	temp_file_error() = {error, {temp_dir, <a href="file.md#type-posix">file:posix()</a>}}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#can_read-1">can_read/1</a></td><td>Test if file or directory is readable.</td></tr><tr><td valign="top"><a href="#can_write-1">can_write/1</a></td><td>Test if file or directory is writeable.</td></tr><tr><td valign="top"><a href="#find_parallel_folder-3">find_parallel_folder/3</a></td><td>Walk absolute directory path, finding where parallel would occur.</td></tr><tr><td valign="top"><a href="#last_modified-1">last_modified/1</a></td><td>Get last date and time file last modified.</td></tr><tr><td valign="top"><a href="#realname-1">realname/1</a></td><td>Ascend absolute path of file relative to current working directory, to
obtain its canonical system path.</td></tr><tr><td valign="top"><a href="#realname-2">realname/2</a></td><td>Ascend absolute path of a file relative to a given directory, to obtain
its canonical system path.</td></tr><tr><td valign="top"><a href="#size-1">size/1</a></td><td>Get current size of file.</td></tr><tr><td valign="top"><a href="#tempdir-0">tempdir/0</a></td><td>Get system temporary directory.</td></tr><tr><td valign="top"><a href="#tempname-0">tempname/0</a></td><td>Get a unique name for a temporary file in the system temporary
directory.</td></tr><tr><td valign="top"><a href="#tempname-1">tempname/1</a></td><td>Get a unique name for a temporary file in the specified directory.</td></tr><tr><td valign="top"><a href="#trim-1">trim/1</a></td><td>Strip whitespace characters from both ends of string.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="can_read-1"></a>

###can_read/1##


	can_read(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -> <a href="#type-permissions_return">permissions_return()</a>
<br></br>


Test if file or directory is readable.<a name="can_write-1"></a>

###can_write/1##


	can_write(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -> <a href="#type-permissions_return">permissions_return()</a>
<br></br>


Test if file or directory is writeable.<a name="find_parallel_folder-3"></a>

###find_parallel_folder/3##


	find_parallel_folder(OldFlder::<a href="#type-folder">folder()</a>, NewFolder::<a href="#type-folder">folder()</a>, OldPath::<a href="#type-path">path()</a>) -> <a href="#type-parallel_result">parallel_result()</a>
<br></br>


Walk absolute directory path, finding where parallel would occur.<a name="last_modified-1"></a>

###last_modified/1##


	last_modified(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -> <a href="#type-datestamp_return">datestamp_return()</a>
<br></br>


Get last date and time file last modified.<a name="realname-1"></a>

###realname/1##


	realname(File::<a href="file.md#type-filename_all">file:filename_all()</a>) -> <a href="#type-path_string">path_string()</a>
<br></br>


Ascend absolute path of file relative to current working directory, to
obtain its canonical system path.<a name="realname-2"></a>

###realname/2##


	realname(File::<a href="file.md#type-filename_all">file:filename_all()</a>, Dir::<a href="file.md#type-filename_all">file:filename_all()</a>) -> <a href="#type-realname_result">realname_result()</a>
<br></br>


Ascend absolute path of a file relative to a given directory, to obtain
its canonical system path.<a name="size-1"></a>

###size/1##


	size(Filename::<a href="file.md#type-name_all">file:name_all()</a>) -> <a href="#type-size_return">size_return()</a>
<br></br>


Get current size of file.<a name="tempdir-0"></a>

###tempdir/0##


	tempdir() -> {ok, <a href="file.md#type-filename">file:filename()</a>} | {error, <a href="file.md#type-posix">file:posix()</a>}
<br></br>


Get system temporary directory.<a name="tempname-0"></a>

###tempname/0##


	tempname() -> {ok, <a href="file.md#type-filename">file:filename()</a>} | <a href="#type-temp_file_error">temp_file_error()</a>
<br></br>


Get a unique name for a temporary file in the system temporary
directory.<a name="tempname-1"></a>

###tempname/1##


	tempname(Dir::<a href="file.md#type-filename">file:filename()</a>) -> {ok, <a href="file.md#type-filename_all">file:filename_all()</a>}
<br></br>


Get a unique name for a temporary file in the specified directory.<a name="trim-1"></a>

###trim/1##


	trim(String::string()) -&gt; string()
<br></br>


Strip whitespace characters from both ends of string.

__<font color="red">To do</font>__
<br></br>
* <font color="red">figure out better place for this to live</font>
