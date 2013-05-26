

#Module pose_syntax#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Erlang syntax utility functions.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.1.1

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-filename">filename()</a>##



	filename() = <a href="file.md#type-filename">file:filename()</a>



###<a name="type-node_data">node_data()</a>##



	node_data() = term()



###<a name="type-syntax_type">syntax_type()</a>##



	syntax_type() = atom()
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#functions-1">functions/1</a></td><td>List all functions defined in module.</td></tr><tr><td valign="top"><a href="#harvest-2">harvest/2</a></td><td>List all items of a specific type in an Erlang source file.</td></tr><tr><td valign="top"><a href="#qualifiers-1">qualifiers/1</a></td><td>Uniquely list all modules that qualify function calls.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="functions-1"></a>

###functions/1##


	functions(File::<a href="#type-filename">filename()</a>) -> [{module(), arity()}]
<br></br>


List all functions defined in module.<a name="harvest-2"></a>

###harvest/2##


	harvest(Type::<a href="#type-syntax_type">syntax_type()</a>, File::<a href="#type-filename">filename()</a>) -> [<a href="#type-node_data">node_data()</a>]
<br></br>


List all items of a specific type in an Erlang source file.<a name="qualifiers-1"></a>

###qualifiers/1##


	qualifiers(File::<a href="#type-filename">filename()</a>) -> [module()]
<br></br>


Uniquely list all modules that qualify function calls.