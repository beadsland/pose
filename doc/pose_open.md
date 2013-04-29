

#Module pose_open#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


A `pose`-command interface for reading and writing files in
local filesystem.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.4

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



	modes() = [<a href="file.md#type-mode">file:mode()</a> | ram | dos]
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#file-2">file/2</a></td><td></td></tr><tr><td valign="top"><a href="#reader-1">reader/1</a></td><td></td></tr><tr><td valign="top"><a href="#reader-2">reader/2</a></td><td></td></tr><tr><td valign="top"><a href="#writer-1">writer/1</a></td><td></td></tr><tr><td valign="top"><a href="#writer-2">writer/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="file-2"></a>

###file/2##


	file(File::<a href="#type-filename">filename()</a>, Modes::<a href="#type-modes">modes()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


<a name="reader-1"></a>

###reader/1##


	reader(File::<a href="#type-filename">filename()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


<a name="reader-2"></a>

###reader/2##


	reader(File::<a href="#type-filename">filename()</a>, Modes::<a href="#type-modes">modes()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


<a name="writer-1"></a>

###writer/1##


	writer(File::<a href="#type-filename">filename()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


<a name="writer-2"></a>

###writer/2##


	writer(File::<a href="#type-filename">filename()</a>, Modes::<a href="#type-modes">modes()</a>) -> <a href="#type-file_pid">file_pid()</a>
<br></br>


