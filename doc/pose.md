

#Module pose#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Entry points for running `pose`-compatible commands.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.8

__Behaviours:__ [`gen_command`](gen_command.md).

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



	command() = atom() | string()



###<a name="type-directory">directory()</a>##



	directory() = string()



###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}



###<a name="type-path_list">path_list()</a>##



	path_list() = [<a href="#type-directory">directory()</a>]



###<a name="type-warning">warning()</a>##



	warning() = <a href="pose_command.md#type-load_mod_warn">pose_command:load_mod_warn()</a>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#deps-0">deps/0</a></td><td>Return project subdirectory in which project dependencies are found.</td></tr><tr><td valign="top"><a href="#env-0">env/0</a></td><td>Return a record of all <code>pose</code> process environment variables.</td></tr><tr><td valign="top"><a href="#env-1">env/1</a></td><td>Return a value among the <code>pose</code> process environment variables.</td></tr><tr><td valign="top"><a href="#init-2">init/2</a></td><td>Initialize <code>pose</code> process.</td></tr><tr><td valign="top"><a href="#iwd-0">iwd/0</a></td><td>Return the initial working directory of the Erlang runtime.</td></tr><tr><td valign="top"><a href="#path-0">path/0</a></td><td>Return the current search path for <code>pose</code> command modules.</td></tr><tr><td valign="top"><a href="#run-3">run/3</a></td><td></td></tr><tr><td valign="top"><a href="#send_load_warnings-3">send_load_warnings/3</a></td><td>Send messages to <code>stderr</code> process detailing any warnings received from
<code>pose_command:load/1</code>.</td></tr><tr><td valign="top"><a href="#setenv-2">setenv/2</a></td><td>Assign a value to a <code>pose</code> process environment variable, such that
it will be shared with <code>pose</code> subprocesses that inherit the environment.</td></tr><tr><td valign="top"><a href="#setpath-1">setpath/1</a></td><td>Set the search path for <code>pose</code> command modules.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>Equivalent to <a href="#start-1"><tt>start([])</tt></a>.</td></tr><tr><td valign="top"><a href="#start-1">start/1</a></td><td>Start as a blocking function.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="deps-0"></a>

###deps/0##


	deps() -&gt; string()
<br></br>


Return project subdirectory in which project dependencies are found.<a name="env-0"></a>

###env/0##


	env() -> #env{all = [<a href="#type-env_prop">env_prop()</a>]}
<br></br>


Return a record of all `pose` process environment variables.<a name="env-1"></a>

###env/1##


	env(Key::atom()) -&gt; term()
<br></br>


Return a value among the `pose` process environment variables.<a name="init-2"></a>

###init/2##


	init(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, ENV::#env{all = [<a href="#type-env_prop">env_prop()</a>]}) -> ok
<br></br>


Initialize `pose` process.<a name="iwd-0"></a>

###iwd/0##


	iwd() -&gt; string()
<br></br>


Return the initial working directory of the Erlang runtime.<a name="path-0"></a>

###path/0##


	path() -> <a href="#type-path_list">path_list()</a>
<br></br>


Return the current search path for `pose` command modules.<a name="run-3"></a>

###run/3##


	run(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{all = [<a href="#type-env_prop">env_prop()</a>]}) -> no_return()
<br></br>


<a name="send_load_warnings-3"></a>

###send_load_warnings/3##


	send_load_warnings(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Command::<a href="#type-command">command()</a>, Warnings::[<a href="#type-warning">warning()</a>]) -> ok
<br></br>


Send messages to `stderr` process detailing any warnings received from
`pose_command:load/1`.  Flat package errors are consolidated if more than
one, or dropped, if Erlang/OTP release does not support packages.<a name="setenv-2"></a>

###setenv/2##


	setenv(Key::atom(), Value::term()) -&gt; term()
<br></br>


Assign a value to a `pose` process environment variable, such that
it will be shared with `pose` subprocesses that inherit the environment.<a name="setpath-1"></a>

###setpath/1##


	setpath(Path::<a href="#type-path_list">path_list()</a>) -> <a href="#type-path_list">path_list()</a>
<br></br>


Set the search path for `pose` command modules.<a name="start-0"></a>

###start/0##


	start() -&gt; no_return()
<br></br>


Equivalent to [`start([])`](#start-1).<a name="start-1"></a>

###start/1##


	start(Param::[atom()]) -&gt; no_return()
<br></br>


Start as a blocking function.