

#Module ?module#

* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)


Posure package import checker for pose.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.1

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##
 Checks pose-compatible
command modules for unimported library modules.<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#run-3">run/3</a></td><td>Start posure package import check as a
<a href="http://github.com/beadsland/pose">pose</a> process.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Start posure package import check as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="run-3"></a>

###run/3##


<pre>run(IO::#std{}, ARG::#arg{}, ENV::#env{}) -&gt; no_return()</pre>
<br></br>


Start posure package import check as a
[pose](http://github.com/beadsland/pose) process.

__<font color="red">To do</font>__
<br></br>

* <font color="red">Match all fully qualified function calls</font>
* <font color="red">Identify non-imports</font>
* <font color="red">Filter out fellow package modules</font>
<a name="start-0"></a>

###start/0##


<pre>start() -&gt; ok | notsure</pre>
<br></br>


Start posure package import check as a blocking function.
All results are written to standard output.