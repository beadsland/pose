

#Module pose_file#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


File-related utility functions for use by [`pose_code`](pose_code.md).

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__<font color="red">To do</font>__
<br></br>
* <font color="red">spec API functions</font>

<a name="types"></a>

##Data Types##




###<a name="type-date_time">date_time()</a>##



	date_time() = <a href="calendar.md#type-date_time">calendar:date_time()</a>



###<a name="type-file_err">file_err()</a>##



	file_err() = {atom(), <a href="#type-filename">filename()</a>}



###<a name="type-filename">filename()</a>##



	filename() = nonempty_string()



###<a name="type-folder">folder()</a>##



	folder() = nonempty_string()



###<a name="type-last_mod_rtn">last_mod_rtn()</a>##



	last_mod_rtn() = {ok, <a href="#type-date_time">date_time()</a>} | {error, <a href="#type-file_err">file_err()</a>}



###<a name="type-parallel_result">parallel_result()</a>##



	parallel_result() = {false, <a href="#type-path_string">path_string()</a>} | {true, <a href="#type-path_string">path_string()</a>}



###<a name="type-path">path()</a>##



	path() = <a href="#type-path_string">path_string()</a> | <a href="#type-path_list">path_list()</a>



###<a name="type-path_list">path_list()</a>##



	path_list() = {folders, [<a href="#type-folder">folder()</a>]}



###<a name="type-path_string">path_string()</a>##



	path_string() = nonempty_string()
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#can_read-1">can_read/1</a></td><td>Test if file or directory is readable.</td></tr><tr><td valign="top"><a href="#can_write-1">can_write/1</a></td><td>Test if file or directory is writeable.</td></tr><tr><td valign="top"><a href="#find_parallel_folder-3">find_parallel_folder/3</a></td><td>Walk absolute directory path, finding where parallel would occur.</td></tr><tr><td valign="top"><a href="#last_modified-1">last_modified/1</a></td><td>Get last date and time file last modified.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="can_read-1"></a>

###can_read/1##


	can_read(Filename::<a href="#type-filename">filename()</a>) -> boolean() | {error, <a href="#type-file_err">file_err()</a>}
<br></br>


Test if file or directory is readable.<a name="can_write-1"></a>

###can_write/1##


	can_write(Filename::<a href="#type-filename">filename()</a>) -> boolean() | {error, <a href="#type-file_err">file_err()</a>}
<br></br>


Test if file or directory is writeable.<a name="find_parallel_folder-3"></a>

###find_parallel_folder/3##


	find_parallel_folder(OldFlder::<a href="#type-folder">folder()</a>, NewFolder::<a href="#type-folder">folder()</a>, OldPath::<a href="#type-path">path()</a>) -> <a href="#type-parallel_result">parallel_result()</a>
<br></br>


Walk absolute directory path, finding where parallel would occur.<a name="last_modified-1"></a>

###last_modified/1##


	last_modified(Filename::<a href="#type-filename">filename()</a>) -> <a href="#type-last_mod_rtn">last_mod_rtn()</a>
<br></br>


Get last date and time file last modified.