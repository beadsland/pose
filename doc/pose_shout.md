

#Module pose_shout#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Watch for the creation of a text file, tail it until its associated
lock file has been removed, and then exit.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.2

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##


 Used by [`pose_shell`](pose_shell.md) to
receive the `stdout` of a shell command.

Note: Code is not Unicode safe. Assumes all shell commands produce `latin1`
output.
<a name="types"></a>

##Data Types##




###<a name="type-filename">filename()</a>##



	filename() = string()
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#monitor-1">monitor/1</a></td><td>Watch a text file, copying each line written to it to calling process
as standard output, until an associated lock file is removed (this signaling
that writing to the file has finished.).</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="monitor-1"></a>

###monitor/1##


	monitor(File::<a href="#type-filename">filename()</a>) -> pid()
<br></br>


Watch a text file, copying each line written to it to calling process
as standard output, until an associated lock file is removed (this signaling
that writing to the file has finished.)