

#Module pose_command#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Load command modules and submodules in a single operation.

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.8

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-load_err_term">load_err_term()</a>##



	load_err_term() = <a href="pose_code.md#type-load_err">pose_code:load_err()</a> | nopath | {submodules, nopath}



###<a name="type-load_error">load_error()</a>##



	load_error() = {error, <a href="#type-load_err_term">load_err_term()</a>, <a href="#type-load_warnings">load_warnings()</a>}



###<a name="type-load_result">load_result()</a>##



	load_result() = {module, module(), <a href="#type-load_warnings">load_warnings()</a>}



###<a name="type-load_return">load_return()</a>##



	load_return() = <a href="#type-load_result">load_result()</a> | <a href="#type-load_error">load_error()</a>



###<a name="type-load_warnings">load_warnings()</a>##



	load_warnings() = [<a href="pose_code.md#type-load_warn">pose_code:load_warn()</a>]
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Equivalent to <a href="#load_command-1"><tt>load_command(Command)</tt></a>.</td></tr><tr><td valign="top"><a href="#load_command-1">load_command/1</a></td><td>Load a command module and all submodules in the same directory.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="load-1"></a>

###load/1##


	load(Command::<a href="pose.md#type-command">pose:command()</a>) -> <a href="#type-load_return">load_return()</a>
<br></br>


Equivalent to [`load_command(Command)`](#load_command-1).<a name="load_command-1"></a>

###load_command/1##


	load_command(Command::<a href="pose.md#type-command">pose:command()</a>) -> <a href="#type-load_return">load_return()</a>
<br></br>


Load a command module and all submodules in the same directory.
Here, a submodule is indicated by the syntax
`<i>module</i>_<i>subpart</i>`.