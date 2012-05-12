

#Welcome to the POSIX-like interface emulation (pose)#


Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.0

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

This is the POSIX-like interface emulation for use with the[`nosh`](http://github.com/beadsland/nosh) Bourne shell
  emulator and `nosh`-compatible projects.
* [Installation](#Installation)
* [Standard I/O](#Standard_I/O)
* [Macros](#Macros)


###<a name="Installation">Installation</a>##


`pose` should be included in any `nosh`-compatible project as a project
  dependency via `rebar.config`:
<blockquote>
  {deps, [
     {pose, ".*",
       {git, "git://github.com/beadsland/pose", {branch, master}}}
    ]}</blockquote>


Any module that uses `pose` macros should then include the `pose`
interface header file:
<blockquote>
  %-define(debug, true).
<br></br>

  -include("pose/include/interface.hrl").</blockquote>


(The `debug` macro definition can be uncommented when `pose` debugging
  output is to be included on `stderr`.)

Any application using the `pose` interface must initialize `pose`
  with the `?INIT_POSE` macro before other macros can be used.

Any process using the debugging feature must additionally call the`?INIT_DEBUG` macro.

###<a name="Standard_I/O">Standard I/O</a>##


Processes using the `pose` interface simulate POSIX-style
  standard I/O streams using the `#std{}` record.  The convention is to
  pass this record as the first parameter to functions performing or
  implementing I/O functionality as `IO`, as in:
<blockquote>
  loop(IO, ...)</blockquote>


Each I/O stream is a process that sends (in the case of `stdin`) or
  receives (in the case of `stdout` and `stderr`) messages, such that
message queues simulate POSIX buffers.  The pids of each standard I/O
process can be addressed as follows:

<dt> Standard input </dt>

 

<dd> <code>IO#std.in</code> </dd>



<dt> Standard output </dt>

 

<dd> <code>IO#std.out</code> </dd>



<dt> Standard error </dt>

 

<dd> <code>IO#std.err</code> </dd>



Messages from and to `pose` processes are of the form`{Tag, SendingPid, Data}` and may be tagged as follows.

<table><tr><td> <code>stdout</code> </td><td> A <code>string()</code> for printing by the console process.</td></tr><tr><td> <code>stderr</code> </td><td> A <code>string()</code> for printing as an error message by the console
       process. </td></tr><tr><td> <code>debug</code> </td><td> A <code>string()</code> for printing as an debug message by the console process.</td></tr><tr><td> <code>erlout</code> </td><td> An <code>atom()</code> or <code>tuple()</code> for pretty printing by the console process.</td></tr><tr><td> <code>erlerr</code> </td><td> An <code>atom()</code> or <code>tuple()</code> for pretty printing as an error message
       by the console process. </td></tr></table>



###<a name="Macros">Macros</a>##


The `pose` interface provides a number of macros for use by`pose`-compatible modules.

<table><tr><td> <code>?INIT_POSE</code> </td><td> Initialize the <code>pose</code> interface.  Must be called when any
       pose-compatible application starts. </td></tr><tr><td> <code>?INIT_DEBUG</code> </td><td> Initialize the <code>pose</code> debug stream.  Must be called at the
       beginning of any pose-compatible process. </td></tr><tr><td> <code>?IO(Pid :: pid())</code> </td><td> Create a <code>pose</code> I/O record, setting each of <code>stdin</code>, <code>stdout</code> and<code>stderr</code> to Pid </td></tr></table>

_Draft documentation to be finished tomorrow._

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="pose_stdio.md" class="module">pose_stdio</a></td></tr></table>

