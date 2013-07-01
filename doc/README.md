

#Welcome to the POSIX-like interface emulation (pose)#


Copyright (c) 2012, 2013 Beads D. Land-Trujillo

__Version:__ 0.1.3

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__* For a project using `pose`, see
[`nosh`](http://github.com/beadsland/nosh).
* This project and projects that depend on it are developed under
[Erlang/OTP R15B01](http://www.erlang.org/download_release/14),
to take advantage of the experimental `package` feature.  (Support for
packages was removed from Erlang/OTP as of R16A01.)


__<font color="red">To do</font>__
<br></br>
* <font color="red">desktop: snow leopard iso</font>
* <font color="red">desktop: dskchk</font>
* <font color="red">desktop: freezes</font>
* <font color="red">vaio: mcafee: uninstall utilty</font>
* <font color="red">vaio: adaware</font>
* <font color="red">vaio: defrag</font>
* <font color="red">vaio: updates</font>
* <font color="red">...plus 197 more (see TODO.edoc)
</font>


This is the POSIX-like interface emulation for use with the
[`nosh`](http://github.com/beadsland/nosh) Bourne shell
  emulator and `nosh`-compatible projects.
 

* [Installation](#Installation)

* [Just-in-Time Packaging](#Just-in-Time_Packaging)

* [Standard I/O](#Standard_I/O)

* [Macros](#Macros)


 


###<a name="Installation">Installation</a>##

 


`pose` should be included in any `pose`-compatible project as a project
  dependency via `rebar.config`:
 
	
	  {deps, [
	     {pose, ".*",
	       {git, "git://github.com/beadsland/pose", {branch, master}}}
	    ]}

 


Any module that uses `pose` macros should then include the `pose`
interface header file:
 
	
	  %-define(debug, true).
	  -include_lib("pose/include/interface.hrl").

 


(The `debug` macro definition can be uncommented when `pose` debugging
  output is to be included on `stderr`.)
 


###<a name="Just-in-Time_Packaging">Just-in-Time Packaging</a>##

 


The `pose` project provides for just-in-time packaging (JITP) as part of
  the functionality of [`pose_code`](pose_code.md).  Packages were an experimental
  feature of Erlang/OTP officially removed as of R16A01.  This project and
  projects dependent on it are therefore developed to run under
[Erlang/OTP R15B01](http://www.erlang.org/download_release/14),
but should be compatible with later releases.
 


When fully implemented, `pose` JITP will allow for the arbitrary loading
  and concurrent operation of likenamed modules that participate in the
`pose` architecture.  This will allow for seamless testing and comparison
  of derivative and variant code within the same runtime system.
  Meanwhile, as packages are only assigned to `pose`-compatible modules at
  compile time, existing Erlang development tools need know nothing about
  packages to work with `pose`-compatible modules, nor need packages be
supported when such code is put into production.
 


###<a name="Standard_I/O">Standard I/O</a>##

 


Processes using the `pose` interface simulate POSIX-style
  standard I/O streams using the `#std{}` record.  The convention is to
  pass this record as the first parameter to functions performing or
  implementing I/O functionality as `IO`, as in:
 
	
	  loop(IO, ...)

 


Each I/O stream is a process that sends (in the case of `stdin`) or
  receives (in the case of `stdout` and `stderr`) messages, such that
message queues simulate POSIX buffers.  The pids of each standard I/O
process can be addressed as follows:
 


<table>
<tr><td> Standard input </td> <td> <code>IO#std.in</code> </td></tr>
<tr><td> Standard output </td> <td> <code>IO#std.out</code> </td></tr>
<tr><td> Standard error </td> <td> <code>IO#std.err</code> </td></tr>
</table>


 


Messages from and to `pose` processes are of the form
`{Tag, SendingPid, Data}` and may be tagged as follows.
 


<table>
<tr><td> <code>stdout</code> </td>
<td> A <code>string()</code> for printing by the console process.
</td></tr>
<tr><td> <code>stderr</code> </td>
<td> A <code>string()</code> for printing as an error message by the console
       process. </td></tr>
<tr><td> <code>debug</code> </td>
<td> A <code>string()</code> for printing as an debug message by the console process.
</td></tr>
<tr><td> <code>erlout</code> </td>
<td> An <code>atom()</code> or <code>tuple()</code> for pretty printing by the console process.
</td></tr>
<tr><td> <code>erlerr</code> </td>
<td> An <code>atom()</code> or <code>tuple()</code> for pretty printing as an error message
       by the console process. </td></tr>
</table>


 


###<a name="Macros">Macros</a>##

 


The `pose` interface provides a number of macros for use by
`pose`-compatible modules.
 


####<a name="Process_Run">Process Run</a>##

 


<table>
<tr><td> <code>?INIT_POSE</code> </td>
<td> Initialize the <code>pose</code> interface.  Must be called by entry-function
       of any <code>pose</code>-compatible process. </td></tr>
<tr><td> <code>?IO(Pid :: pid()) -> #std{}</code> </td>
<td> Create a <code>pose</code> I/O record, setting each of <code>stdin</code>, <code>stdout</code> and
<code>stderr</code> to Pid </td></tr>
<tr><td> <code>?IO(In :: pid(), Out :: pid(), Err :: pid()) -> #std{}</code> </td>
<td> Create a <code>pose</code> I/O record. </td></tr>
<tr><td width="30%"> <code>?IO(In :: pid(), Out :: pid(), Err :: pid(),
       Echo :: boolean()) -> #std{}</code> </td>
<td> <i>Deprecated.</i>  Create a <code>pose</code> IO record, setting the echo
       flag.  Setting the flag to <code>true</code> indicates that the process run
       with this IO record should echo <code>stdin</code> back to <code>stdout</code>.  Defaults
       to false. </td></tr>
<tr><td> <code>?ARG(Command :: command(), Values :: [any()]) -> #arg{}</code></td>
<td> Create a <code>pose</code> Arg record. </td></tr>
<tr><td> <code>?ARG(Command :: command()) -> #arg{}</code></td>
<td> Create a <code>pose</code> Arg record, with no arguments. </td></tr>
<tr><td> <code>?ARGV(N) -> any()</code> </td>
<td> Return argument <i>N</i>.  Argument 0 corresponds to the name of
       the command run to start the current process.  Only available
       in functions that receive an <code>ARG</code> parameter. </td></tr>
<tr><td> <code>?ENV -> #env{}</code> </td>
<td> Create a <code>pose</code> Env record, setting it to hold the global
       environment values of the current process. </td></tr>
</table>


 


####<a name="Output_and_Error_Output">Output and Error Output</a>##

 


<table>
<tr><td> <code>?STDOUT(Format :: format(), What :: list())</code> </td>
<td> Send a <code>io_lib:format/2` formatted `string()</code> message to <code>stdout</code>
       process.
<i>Only available to functions with an IO parameter.</i></td></tr>
<tr><td> <code>?STDOUT(What :: string() | tuple() | atom())</code> </td>
<td> Send either a <code>string()</code> message to <code>stdout</code> process or an <code>erlout</code>
       tagged message to the same process.
<i>Only available to functions with an IO parameter.</i></td></tr>
<tr><td> <code>?STDERR(Format :: format(), What :: list())</code> </td>
<td> Send a <code>io_lib:format/2` formatted `string()</code> message to <code>stderr</code>
       process.
<i>Only available to functions with an IO parameter.</i></td></tr>
<tr><td> <code>?STDERR(What :: string() | tuple() | atom())</code> </td>
<td> Send either a <code>string()</code> message to <code>stderr</code> process or an <code>erlerr</code>
       tagged message to the same process.
<i>Only available to functions with an IO parameter.</i></td></tr>
<tr><td> <code>?ERLERR_FORMAT(What :: any()) -> string()</code> </td>
<td> Format <code>erlerr</code> type message data as an easy-to-read string for
       printing to console. </td></tr>
<tr><td> <code>?DEBUG(Format :: format(), What :: list())</code> </td>
<td> If <code>debug</code> macro is <code>true</code>, send an <code>io_lib:format/2</code> formatted,
<code>debug</code> tagged message to <code>stderr</code> process.  Otherwise, quietly
       do nothing. <i>Does not require IO parameter.</i></td></tr>
<tr><td> <code>?DEBUG(What :: string())</code> </td>
<td> If <code>debug</code> macro is <code>true</code>, send a <code>debug</code> tagged message to
<code>stderr</code> process.  Otherwise, quietly do nothing.
<i>Does not require IO parameter.</i></td></tr>
</table>


 

##Modules##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="gen_command.md" class="module">gen_command</a></td></tr>
<tr><td><a href="pose.md" class="module">pose</a></td></tr>
<tr><td><a href="pose_beam.md" class="module">pose_beam</a></td></tr>
<tr><td><a href="pose_code.md" class="module">pose_code</a></td></tr>
<tr><td><a href="pose_command.md" class="module">pose_command</a></td></tr>
<tr><td><a href="pose_compile.md" class="module">pose_compile</a></td></tr>
<tr><td><a href="pose_file.md" class="module">pose_file</a></td></tr>
<tr><td><a href="pose_open.md" class="module">pose_open</a></td></tr>
<tr><td><a href="pose_package.md" class="module">pose_package</a></td></tr>
<tr><td><a href="pose_shell.md" class="module">pose_shell</a></td></tr>
<tr><td><a href="pose_short.md" class="module">pose_short</a></td></tr>
<tr><td><a href="pose_shout.md" class="module">pose_shout</a></td></tr>
<tr><td><a href="pose_stdio.md" class="module">pose_stdio</a></td></tr>
<tr><td><a href="pose_syntax.md" class="module">pose_syntax</a></td></tr>
<tr><td><a href="posure.md" class="module">posure</a></td></tr></table>

