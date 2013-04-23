

#Module pose_command#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Load command modules and submodules in a single operation.

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.7

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-load_cmd_rtn">load_cmd_rtn()</a>##



	load_cmd_rtn() = {module, module(), [<a href="#type-load_mod_warn">load_mod_warn()</a>]} | {error, <a href="#type-load_err">load_err()</a>, [<a href="#type-load_mod_warn">load_mod_warn()</a>]}



###<a name="type-load_err">load_err()</a>##



	load_err() = <a href="pose_code.md#type-load_err">pose_code:load_err()</a>



###<a name="type-load_mod_warn">load_mod_warn()</a>##



	load_mod_warn() = {module(), <a href="#type-load_warn">load_warn()</a>} | <a href="#type-load_warn">load_warn()</a>



###<a name="type-load_warn">load_warn()</a>##



	load_warn() = <a href="pose_code.md#type-load_warn">pose_code:load_warn()</a>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-1">load/1</a></td><td>Equivalent to <a href="#load_command-1"><tt>load_command(Command)</tt></a>.</td></tr><tr><td valign="top"><a href="#load_command-1">load_command/1</a></td><td>Load a command module and all submodules in the same directory.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="load-1"></a>

###load/1##


	load(Command::<a href="pose.md#type-command">pose:command()</a>) -> <a href="#type-load_cmd_rtn">load_cmd_rtn()</a>
<br></br>


Equivalent to [`load_command(Command)`](#load_command-1).<a name="load_command-1"></a>

###load_command/1##


	load_command(Command::<a href="pose.md#type-command">pose:command()</a>) -> <a href="#type-load_cmd_rtn">load_cmd_rtn()</a>
<br></br>


Load a command module and all submodules in the same directory.
Here, a submodule is indicated by the syntax
`<i>module</i>_<i>subpart</i>`.