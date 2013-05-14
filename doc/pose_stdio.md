

#Module pose_stdio#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Standard I/O functions underlying `interface.hrl` macros.

Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.2.7

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-format">format()</a>##



	format() = <a href="io.md#type-format">io:format()</a>



###<a name="type-output">output()</a>##



	output() = {atom(), any()} | atom() | string()
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format_erlerr-1">format_erlerr/1</a></td><td>Smartly format erlerr messages.</td></tr><tr><td valign="top"><a href="#send_debug-1">send_debug/1</a></td><td>Smart DEBUG/1 macro function.</td></tr><tr><td valign="top"><a href="#send_debug-2">send_debug/2</a></td><td>Smart DEBUG/2 macro function.</td></tr><tr><td valign="top"><a href="#send_stderr-2">send_stderr/2</a></td><td>Smart STDERR/1 macro function.</td></tr><tr><td valign="top"><a href="#send_stderr-3">send_stderr/3</a></td><td>Smart STDERR/2 macro function.</td></tr><tr><td valign="top"><a href="#send_stdout-2">send_stdout/2</a></td><td>Smart STDOUT/1 macro function.</td></tr><tr><td valign="top"><a href="#send_stdout-3">send_stdout/3</a></td><td>Smart STDOUT/2 macro function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format_erlerr-1"></a>

###format_erlerr/1##


	format_erlerr(What::any()) -&gt; string()
<br></br>


Smartly format erlerr messages.<a name="send_debug-1"></a>

###send_debug/1##


	send_debug(Output::any()) -&gt; ok
<br></br>


Smart DEBUG/1 macro function.<a name="send_debug-2"></a>

###send_debug/2##


	send_debug(Format::<a href="#type-format">format()</a>, What::list()) -> ok
<br></br>


Smart DEBUG/2 macro function.<a name="send_stderr-2"></a>

###send_stderr/2##


	send_stderr(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Output::<a href="#type-output">output()</a>) -> ok
<br></br>


Smart STDERR/1 macro function.<a name="send_stderr-3"></a>

###send_stderr/3##


	send_stderr(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Format::<a href="#type-format">format()</a>, What::list()) -> ok
<br></br>


Smart STDERR/2 macro function.<a name="send_stdout-2"></a>

###send_stdout/2##


	send_stdout(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Output::<a href="#type-output">output()</a>) -> ok
<br></br>


Smart STDOUT/1 macro function.<a name="send_stdout-3"></a>

###send_stdout/3##


	send_stdout(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Format::<a href="#type-format">format()</a>, What::list()) -> ok
<br></br>


Smart STDOUT/2 macro function.