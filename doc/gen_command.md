

#Module gen_command#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Generic `pose` command behaviour.

Copyright (c) 2012 Beads D. Land-Trujillo

__This module defines the `gen_command` behaviour.__
<br></br>
 Required callback functions: `start/0`, `start/1`, `do_run/2`.

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-callback">callback()</a>##



<pre>callback() = {function(), arity()}</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#run-4">run/4</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="behaviour_info-1"></a>

###behaviour_info/1##


<pre>behaviour_info(X1::callbacks) -> [<a href="#type-callback">callback()</a>] | undefined</pre>
<br></br>


<a name="run-4"></a>

###run/4##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}, Module::module()) -&gt; no_return()</pre>
<br></br>


<a name="start-2"></a>

###start/2##


<pre>start(Param::[any()], Module::module()) -&gt; no_return()</pre>
<br></br>


