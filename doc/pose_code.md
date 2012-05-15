

#Module pose_code#

* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)


Intuitive module loader.

Copyright (c) 2012 Beads D. Land-Trujillo

__Version:__ 0.1.6

__Authors:__ Beads D. Land-Trujillo (_web site:_ [`http://twitter.com/beadsland`](http://twitter.com/beadsland)).

__References__
* For a project using `pose`, see
[`nosh`](http://github.com/beadsland/nosh).
* See discussion of
[Packages in
Erlang](http://www.erlang.se/publications/packages.md).


__<font color="red">To do</font>__
<br></br>

* <font color="red"> module binary service (to avoid repetitive slurps)</font>
* <font color="red"> conservative module loader (to preserve against collisions)</font>
<a name="description"></a>

##Description##




* [Basic Load Process](#Basic_Load_Process)

* [Purge Handling](#Purge_Handling)

* [Warnings](#Warnings)

* [Packaged Modules](#Packaged_Modules)

* [Pose Namespace](#Pose_Namespace)





###<a name="Basic_Load_Process">Basic Load Process</a>##




Each Erlang module is treated as a potential executable command in `pose`.
A call to `pose_code:load/1` results in a search of the directories
listed on the current `PATH` environment variable, with a twist:



For each directory on
`PATH` that ends in `ebin\`, and for which the current user has write
access, `pose` will look for a parallel `src\` directory, and if found,
search for a matching `.erl` file therein.



If an associated `.erl` file is found, and it is newer that the `.beam`
file, or if an `.erl` file is found for which no `.beam` file appears,
the `.erl` file will be compiled to its `ebin\` directory.  If this
compilation is successful, the module will be loaded.
Otherwise, an error is returned.



If no associated `.erl` file is found, the `.beam` file on the `PATH`
is loaded.  If no `.beam` file is found, the search continues to the
next directory on `PATH`, returning an error if no `.beam` file can be
found or compiled from source before the `PATH` is exhausted.



###<a name="Purge_Handling">Purge Handling</a>##




Whenever a new binary is obtained by `pose_code`, a `code:soft_purge/1`
is called, and on a `true` result, current code for the binary is made
old (`code:delete/1`) and the binary is loaded as current code.



In the event of a `false` result from `code:soft_purge/1`, a message is
broadcast to all active processes of the form
`{purging, PurgePid, Module}`, where 'PurgePid' is the `pid()` of the
process initiating the purge, and 'Module' is the atom identifying the
module to be purged.



In order to take advantage of this broadcast, and escape being killed
for lingering in old code, `pose`-compatible modules should begin with
a case clause in message loops to respond to `purging` messages with a
fully-qualified call to the loop function.  As per the following example:

<pre>
  loop(...) ->
    receive
      {purging, _Pid, _Mod} -> ?MODULE:loop(...);
                    *     *     *
    end.</pre>



###<a name="Warnings">Warnings</a>##




Load may return successfully with either a 2-tuple, `{module, Module}`
or a 3-tuple `{module, Module, Warning}`.  In the later case, the
`Warning` may be either of:



<table>
<tr><td> <code>flat_pkg</code> </td>
<td> The module was compiled under Erlang's flat namespace, and no
<code>-package</code> directive was found indicating that <code>pose</code> could
recompile the module under the <a href="#Pose_Namespace">Pose Namespace</a>.
Flat-package modules are considered unsafe, as there may be
other module binaries or source files with the same name elsewhere
in the file system.</td></tr>
<tr><td> <code>diff_path</code> </td>
<td> The module was compiled under a namespace that matches the namespace
of old code loaded from a different file path.  That is, the current
and old code in the system originate from different points in the
file system.  Such a <i>namespace collision</i> can occur when
flat-package modules with the same name are loaded from different
points in the file system.</td></tr>
</table>





###<a name="Packaged_Modules">Packaged Modules</a>##




Erlang provides for namespace management through an experimental
packages feature.  As implemented in Erlang, the package of a module
is expressed as a dot-separated path in the `-module` directive.
For instance, a package `fum` in the `fee.foo` package (where `fee.foo`
is a subpackage of `fee`), would be declared as:

<pre>
  -module(fee.foo.fum).</pre>



The package hierarchy, in turn, corresponds to the file hierarchy of
a module relative to the current code path.  So, continuing our example,
if the current code path includes `/home/user/project/ebin`, the
compiled `fee.foo.fum` module would traditionally be sought at
`/home/user/project/ebin/fee/foo/fum.beam`.



###<a name="Pose_Namespace">Pose Namespace</a>##




Unlike standard Erlang, `pose` looks for a module by unpackaged filename,
and upon finding such a file, loads it, returning the fully-qualified
packaged module name.  This means that `pose` would look for `fum` (per
our example above), as `/home/user/project/ebin/fum.beam`, and then
upon successfully loading same, would return
`{module, 'fee.foo.fum'}`.



Additionally, `pose` uses a `-package` directive to identify
`pose`-compatible files that have been compiled in the flat namespace
standard to Erlang and then recompile those files with a package
assigned by `pose` so as to ensure that each such package is uniqely
identified in the namespace of the currently running node.



Users can take advantage of the `-package` directive by including the
following pattern in their `pose`-compatible modules.

<pre>
  -define(module, fum).
  % BEGIN POSE PACKAGE PATTERN
  -ifndef(package).
  -module(?module).
  -package(default).
  -else.
  -module(?package.?module).
  -package(?package).
  -endif.
  % END POSE PACKAGE PATTERN</pre>



When `pose` sees that a module has been compiled with a `-package`
attribute of `default`, it recompiles the module with the macro `?package`
set to a path unique to that module and the other modules in the same
directory.

This allows modules to be developed in the flat namespace recognized by
all existing Erlang development tools, while ensuring that those same
modules will run in their own unique namespace when loaded in a
`pose`-compatible system.
<a name="types"></a>

##Data Types##




###<a name="type-command">command()</a>##



<pre>command() = <a href="pose.md#type-command">pose:command()</a></pre>



###<a name="type-error">error()</a>##



<pre>error() = atom() | {atom(), <a href="#type-error">error()</a>}</pre>



###<a name="type-load_err">load_err()</a>##



<pre>load_err() = {load, <a href="#type-error">error()</a>} | {slurp, <a href="#type-error">error()</a>} | <a href="#type-error">error()</a></pre>



###<a name="type-load_rtn">load_rtn()</a>##



<pre>load_rtn() = {module, module()} | {module, module(), <a href="#type-load_warn">load_warn()</a>} | {error, <a href="#type-load_err">load_err()</a>}</pre>



###<a name="type-load_warn">load_warn()</a>##



<pre>load_warn() = diff_path | flat_pkg</pre>
<a name="index"></a>

##Function Index##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#load-1">load/1</a></td><td>(<em>Deprecated</em>.) Equivalent to <a href="#load_module-1"><tt>load_module(Command)</tt></a>.</td></tr><tr><td valign="top"><a href="#load_module-1">load_module/1</a></td><td>Locate command on <code>PATH</code>, compiling and loading updated module
as necessary.</td></tr></table>


<a name="functions"></a>

##Function Details##

<a name="load-1"></a>

###load/1##


<pre>load(Command::<a href="#type-command">command()</a>) -> <a href="#type-load_rtn">load_rtn()</a></pre>
<br></br>


Equivalent to [`load_module(Command)`](#load_module-1).

__This function is deprecated:__ should be load_command or load_project<a name="load_module-1"></a>

###load_module/1##


<pre>load_module(Command::<a href="#type-command">command()</a>) -> <a href="#type-load_rtn">load_rtn()</a></pre>
<br></br>


Locate command on `PATH`, compiling and loading updated module
as necessary.

__<font color="red">To do</font>__
<br></br>

* <font color="red">get PATH from environment</font>
