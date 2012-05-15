

#Module pose#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Entry points for running `pose`-compatible commands.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.6

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



<pre>command() = nonempty_string() | atom()</pre>



###<a name="type-spawn_rtn">spawn_rtn()</a>##



<pre>spawn_rtn() = {error, <a href="pose_code.md#type-load_err">pose_code:load_err()</a>} | pid()</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#spawn-2">spawn/2</a></td><td>Equivalent to <a href="#spawn-3"><tt>spawn(IO, Command, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#spawn-3">spawn/3</a></td><td>Run a pose-compliant command in its own process.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Run a pose-compliant command from the erl commandline.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="spawn-2"></a>

###spawn/2##


<pre>spawn(IO::#std{}, Command::<a href="#type-command">command()</a>) -> <a href="#type-spawn_rtn">spawn_rtn()</a></pre>
<br></br>


Equivalent to [`spawn(IO, Command, [])`](#spawn-3).<a name="spawn-3"></a>

###spawn/3##


<pre>spawn(IO::#std{}, Command::<a href="#type-command">command()</a>, Param::[any()]) -> <a href="#type-spawn_rtn">spawn_rtn()</a></pre>
<br></br>


Run a pose-compliant command in its own process.<a name="start-1"></a>

###start/1##


<pre>start(Command::[Command::atom()]) -&gt; ok | no_return()</pre>
<br></br>


Run a pose-compliant command from the erl commandline.