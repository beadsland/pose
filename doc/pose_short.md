

#Module pose_short#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Synchronously execute multiple commands under an operating system shell,
short-circuiting on any non-zero exit status.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.2

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-shell_error">shell_error()</a>##



	shell_error() = {error, <a href="#type-shell_error_item">shell_error_item()</a> | <a href="#type-shell_error_tuple">shell_error_tuple()</a>}



###<a name="type-shell_error_item">shell_error_item()</a>##



	shell_error_item() = atom() | pid() | port() | integer() | string()



###<a name="type-shell_error_term">shell_error_term()</a>##



	shell_error_term() = <a href="#type-shell_error_item">shell_error_item()</a> | <a href="#type-shell_error_tuple">shell_error_tuple()</a>



###<a name="type-shell_error_tuple">shell_error_tuple()</a>##



	shell_error_tuple() = {<a href="#type-shell_error_item">shell_error_item()</a>, <a href="#type-shell_error_term">shell_error_term()</a>}



###<a name="type-shell_output">shell_output()</a>##



	shell_output() = {ok, [string()]}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#script-1">script/1</a></td><td>Pass a carriage-return delimited sequence of commands to an operating
system shell, returning either an error result on the first non-zero
exit status or a list of carriage-return terminated strings representing
output from the command sequence.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="script-1"></a>

###script/1##


	script(Sequence::string()) -> <a href="#type-shell_output">shell_output()</a> | <a href="#type-shell_error">shell_error()</a>
<br></br>


Pass a carriage-return delimited sequence of commands to an operating
system shell, returning either an error result on the first non-zero
exit status or a list of carriage-return terminated strings representing
output from the command sequence.