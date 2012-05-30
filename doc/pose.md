

#Module pose#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Entry points for running `pose`-compatible commands.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.7

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



	command() = atom() | string()



###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}



###<a name="type-warning">warning()</a>##



	warning() = <a href="pose_command.md#type-load_mod_warn">pose_command:load_mod_warn()</a>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td>Execute a command within the current process.</td></tr><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#send_load_warnings-3">send_load_warnings/3</a></td><td>Send messages to <code>stderr</code> process detailing any warnings received
from <code>pose_command:load/1</code>.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="exec-2"></a>

###exec/2##


	exec(IO::#std{in = pid(), out = pid(), err = pid(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}) -&gt; no_return()
<br></br>


Execute a command within the current process.<a name="run-3"></a>

###run/3##


	run(IO::#std{in = pid(), out = pid(), err = pid(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{plist = [<a href="#type-env_prop">env_prop()</a>]}) -> no_return()
<br></br>


<a name="send_load_warnings-3"></a>

###send_load_warnings/3##


	send_load_warnings(IO::#std{in = pid(), out = pid(), err = pid(), echo = boolean()}, Command::<a href="#type-command">command()</a>, Warnings::[<a href="#type-warning">warning()</a>]) -> ok
<br></br>


Send messages to `stderr` process detailing any warnings received
from `pose_command:load/1`.<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.