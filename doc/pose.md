

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



<pre>command() = nonempty_string() | atom()</pre>



###<a name="type-spawn_rtn">spawn_rtn()</a>##



<pre>spawn_rtn() = {error, <a href="pose_code.md#type-load_err">pose_code:load_err()</a>} | pid()</pre>



###<a name="type-warning">warning()</a>##



<pre>warning() = <a href="pose_command.md#type-load_mod_warn">pose_command:load_mod_warn()</a></pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#exec-2">exec/2</a></td><td>Execute a command within the current process.</td></tr><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#send_load_warnings-3">send_load_warnings/3</a></td><td>Send messages to <code>stderr</code> process detailing any warnings received
from <code>pose_command:load/1</code>.</td></tr><tr><td valign="top"><a href="#spawn-2">spawn/2</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#spawn-3"><tt>spawn(IO, Command, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#spawn-3">spawn/3</a></td><td>Run a pose-compliant command in its own process.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="exec-2"></a>

###exec/2##


<pre>exec(IO::#std{}, ARG::#arg{}) -&gt; no_return()</pre>
<br></br>


Execute a command within the current process.<a name="run-3"></a>

###run/3##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()</pre>
<br></br>


<a name="send_load_warnings-3"></a>

###send_load_warnings/3##


<pre>send_load_warnings(IO::#std{}, Command::<a href="#type-command">command()</a>, Warnings::[<a href="#type-warning">warning()</a>]) -> ok</pre>
<br></br>


Send messages to `stderr` process detailing any warnings received
from `pose_command:load/1`.<a name="spawn-2"></a>

###spawn/2##


<pre>spawn(IO::#std{}, Command::<a href="#type-command">command()</a>) -> <a href="#type-spawn_rtn">spawn_rtn()</a></pre>
<br></br>


Equivalent to [`spawn(IO, Command, [])`](#spawn-3).

__This function is deprecated:__ Only nosh uses this<a name="spawn-3"></a>

###spawn/3##


<pre>spawn(IO::#std{}, Command::<a href="#type-command">command()</a>, Param::[any()]) -> <a href="#type-spawn_rtn">spawn_rtn()</a></pre>
<br></br>


Run a pose-compliant command in its own process.<a name="start-0"></a>

###start/0##


<pre>start() -&gt; no_return()</pre>
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


<pre>start(Param::[atom()]) -&gt; no_return()</pre>
<br></br>


Start as a blocking function.