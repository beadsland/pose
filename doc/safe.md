

#Module safe#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Safely apply io_lib:format/2, catching badarg runtime errors.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.0.1

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-format">format()</a>##



<pre>format() = <a href="io.md#type-format">io:format()</a></pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#format-2">format/2</a></td><td>Return a formatted string, catching any badarg runtime errors.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="format-2"></a>

###format/2##


<pre>format(Format::<a href="#type-format">format()</a>, What::[any()]) -> string()</pre>
<br></br>


Return a formatted string, catching any badarg runtime errors.