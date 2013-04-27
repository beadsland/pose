

#Module pose_file#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


File-related utility functions for use by [`pose_code`](pose_code.md).

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.5

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">spec API functions</font>

<a name="types"></a>

##Data Types##




###<a name="type-date_time">date_time()</a>##



	date_time() = <a href="calendar.md#type-date_time">calendar:date_time()</a>



###<a name="type-datestamp_return">datestamp_return()</a>##



	datestamp_return() = {ok, <a href="#type-date_time">date_time()</a>} | {error, <a href="#type-file_info_error">file_info_error()</a>}



###<a name="type-file_info_error">file_info_error()</a>##



	file_info_error() = {<a href="#type-filename">filename()</a>, <a href="#type-info_error_atom">info_error_atom()</a>}



###<a name="type-filename">filename()</a>##



	filename() = <a href="file.md#type-name_all">file:name_all()</a>



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



	permissions_return() = boolean() | {error, <a href="#type-file_info_error">file_info_error()</a>}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#can_read-1">can_read/1</a></td><td>Test if file or directory is readable.</td></tr><tr><td valign="top"><a href="#can_write-1">can_write/1</a></td><td>Test if file or directory is writeable.</td></tr><tr><td valign="top"><a href="#find_parallel_folder-3">find_parallel_folder/3</a></td><td>Walk absolute directory path, finding where parallel would occur.</td></tr><tr><td valign="top"><a href="#get_temp_dir-0">get_temp_dir/0</a></td><td>Get system temporary directory.</td></tr><tr><td valign="top"><a href="#get_temp_file-0">get_temp_file/0</a></td><td>Get a uniquely named temporary file name.</td></tr><tr><td valign="top"><a href="#last_modified-1">last_modified/1</a></td><td>Get last date and time file last modified.</td></tr><tr><td valign="top"><a href="#realname-2">realname/2</a></td><td>Ascend absolute directory path of file relative to current working
directory, to obtain its canonical system path.</td></tr><tr><td valign="top"><a href="#realname-3">realname/3</a></td><td>Ascend absolute directory path of a file relative to a directory,
to obtain its canonical system path.</td></tr><tr><td valign="top"><a href="#trim-1">trim/1</a></td><td>Strip whitespace characters from both ends of string.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="can_read-1"></a>

###can_read/1##


	can_read(Filename::<a href="#type-filename">filename()</a>) -> <a href="#type-permissions_return">permissions_return()</a>
<br></br>


Test if file or directory is readable.<a name="can_write-1"></a>

###can_write/1##


	can_write(Filename::<a href="#type-filename">filename()</a>) -> <a href="#type-permissions_return">permissions_return()</a>
<br></br>


Test if file or directory is writeable.<a name="find_parallel_folder-3"></a>

###find_parallel_folder/3##


	find_parallel_folder(OldFlder::<a href="#type-folder">folder()</a>, NewFolder::<a href="#type-folder">folder()</a>, OldPath::<a href="#type-path">path()</a>) -> <a href="#type-parallel_result">parallel_result()</a>
<br></br>


Walk absolute directory path, finding where parallel would occur.<a name="get_temp_dir-0"></a>

###get_temp_dir/0##


	get_temp_dir() -> <a href="#type-filename">filename()</a> | {error, <a href="file.md#type-posix">file:posix()</a>}
<br></br>


Get system temporary directory.<a name="get_temp_file-0"></a>

###get_temp_file/0##


	get_temp_file() -> <a href="#type-filename">filename()</a>
<br></br>


Get a uniquely named temporary file name.<a name="last_modified-1"></a>

###last_modified/1##


	last_modified(Filename::<a href="#type-filename">filename()</a>) -> <a href="#type-datestamp_return">datestamp_return()</a>
<br></br>


Get last date and time file last modified.<a name="realname-2"></a>

###realname/2##


	realname(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, File::<a href="#type-path_string">path_string()</a>) -> <a href="#type-path_string">path_string()</a>
<br></br>


Ascend absolute directory path of file relative to current working
directory, to obtain its canonical system path.<a name="realname-3"></a>

###realname/3##


	realname(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, File::<a href="#type-path_string">path_string()</a>, Dir::<a href="#type-folder">folder()</a>) -> <a href="#type-path_string">path_string()</a>
<br></br>


Ascend absolute directory path of a file relative to a directory,
to obtain its canonical system path.<a name="trim-1"></a>

###trim/1##


	trim(String::string()) -&gt; string()
<br></br>


Strip whitespace characters from both ends of string.

__<font color="red">To do</font>__
<br></br>
* <font color="red">figure out better place for this to live</font>
