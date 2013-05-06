

#Module pose_shell#
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Asyncrhonously execute multiple commands under an operating
system shell.

Copyright (c) 2013 Beads D. Land-Trujillo

__Version:__ 0.0.10

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



	command() = string()



###<a name="type-shell_pid">shell_pid()</a>##



	shell_pid() = pid()
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#command-2">command/2</a></td><td>Send a command to an existing shell process.</td></tr><tr><td valign="top"><a href="#exit-1">exit/1</a></td><td>Exit an operating system shell process, upon successful completion of
all previously submitted commands.</td></tr><tr><td valign="top"><a href="#spawn-0">spawn/0</a></td><td>Spawn an operating system shell interface as a new process.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="command-2"></a>

###command/2##


	command(ShellPid::<a href="#type-shell_pid">shell_pid()</a>, Command::<a href="#type-command">command()</a>) -> ok
<br></br>


Send a command to an existing shell process.  Output and error output
are returned as `stdout` and `stderr` messages, respectively.  Any non-zero
exit status will cause shell interface process to exit with an error.<a name="exit-1"></a>

###exit/1##


	exit(ShellPid::<a href="#type-shell_pid">shell_pid()</a>) -> ok
<br></br>


Exit an operating system shell process, upon successful completion of
all previously submitted commands.<a name="spawn-0"></a>

###spawn/0##


	spawn() -> <a href="#type-shell_pid">shell_pid()</a>
<br></br>


Spawn an operating system shell interface as a new process.  Process
will exit with an error on the first command that returns a non-zero exit
status, or else exit with an `ok` upon a successful call to
`pose_shell:exit/1`.