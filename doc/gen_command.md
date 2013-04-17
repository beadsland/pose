

#Module gen_command#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Generic `pose` command behaviour.

Copyright (c) 2012 Beads D. Land-Trujillo

__This module defines the `gen_command` behaviour.__
<br></br>
 Required callback functions: `start/0`, `start/1`, `run/3`, `do_run/2`.

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).<a name="description"></a>

##Description##


 Provides helper functions for
handling command entry points.



Each `pose` compatible command module will use the following pattern
when implementing this behaviour:

	
	  -spec start() -> no_return().
	  %% @equiv start([])
	  start() -> start([]).
	 
	  -spec start(Param :: [atom()]) -> no_return().
	  %% @doc Start as a blocking function.
	  start(Param) -> gen_command:start(Param, ?MODULE).
	 
	  -spec run(IO :: #std{}, ARG :: #arg{}, ENV :: #env{}) -> no_return().
	  %% doc Start as a <code>pose</code> command.
	  run(IO, ARG, ENV) -> gen_command:run(IO, ARG, ENV, ?MODULE).
	 
	  %% @private Callback entry point for gen_command behaviour.
	  do_run(IO, ARG) -> <i>command functionality goes here</i>.


The `do_run/2` function should finish with an `exit/1`, either with
a zero status (`ok` or `{ok, any()}`) or a non-zero status (any other
value.  Note that the implicit default exit status, `normal` is treated
as a non-zero status by `pose`.  Use an explicit `exit(ok)`, instead.
<a name="types"></a>

##Data Types##




###<a name="type-callback">callback()</a>##



	callback() = {function(), arity()}



###<a name="type-env_prop">env_prop()</a>##



	env_prop() = atom() | {atom(), string()}



###<a name="type-load_rtn">load_rtn()</a>##



	load_rtn() = {module, module()} | {error, <a href="pose_code.md#type-load_err">pose_code:load_err()</a>}
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#behaviour_info-1">behaviour_info/1</a></td><td></td></tr><tr><td valign="top"><a href="#load_command-2">load_command/2</a></td><td></td></tr><tr><td valign="top"><a href="#run-4">run/4</a></td><td></td></tr><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="behaviour_info-1"></a>

###behaviour_info/1##


	behaviour_info(X1::callbacks) -> [<a href="#type-callback">callback()</a>] | undefined
<br></br>


<a name="load_command-2"></a>

###load_command/2##


	load_command(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, Command::<a href="pose.md#type-command">pose:command()</a>) -> <a href="#type-load_rtn">load_rtn()</a>
<br></br>


<a name="run-4"></a>

###run/4##


	run(IO::#std{in = pid(), out = pid(), err = pid(), stop = boolean(), echo = boolean()}, ARG::#arg{cmd = atom(), v = list()}, ENV::#env{plist = [<a href="#type-env_prop">env_prop()</a>]}, Module::module()) -> no_return()
<br></br>


<a name="start-2"></a>

###start/2##


	start(Param::[any()], Module::module()) -&gt; no_return()
<br></br>


