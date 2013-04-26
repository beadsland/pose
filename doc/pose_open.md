

#Module pose_open#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


A `pose`-command interface for reading and writing files in
local filesystem.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.3

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##
 Return the Pid of the process that maintains
the active file handle.
<a name="types"></a>

##Data Types##




###<a name="type-file_pid">file_pid()</a>##



	file_pid() = pid()



###<a name="type-filename">filename()</a>##



	filename() = <a href="file.md#type-name_all">file:name_all()</a> | iodata()



###<a name="type-modes">modes()</a>##



	modes() = <a href="file.md#type-mode">file:mode()</a> | ram
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#open-2">open/2</a></td><td></td></tr><tr><td valign="top"><a href="#read-1">read/1</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="open-2"></a>

###open/2##


	open(File::<a href="#type-filename">filename()</a>, Mode::<a href="#type-modes">modes()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


<a name="read-1"></a>

###read/1##


	read(File::<a href="#type-filename">filename()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


