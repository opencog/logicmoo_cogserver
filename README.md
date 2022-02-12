# logicmoo_cogserver

OpenCog logicmoo_cogserver
=================

opencog | singnet
------- | -------
[![CircleCI](https://circleci.com/gh/opencog/logicmoo_cogserver.svg?style=svg)](https://circleci.com/gh/opencog/logicmoo_cogserver) | [![CircleCI](https://circleci.com/gh/singnet/logicmoo_cogserver.svg?style=svg)](https://circleci.com/gh/singnet/logicmoo_cogserver)

The OpenCog Logicmoo Cogserver is a network scheme/prolog command-line
and logicmoo server for the [OpenCog framework](https://opencog.org).

Overview
--------
The logicmoo_cogserver provides a network command-line console server and
a logicmoo narrative server.  The network console server provides a fast,
efficient telnet interface, giving access to Scheme (guile) and
Prolog command-lines.  Both can be used by multiple users at the
same time, all obtaining access to the *same* AtomSpace.  This
is very useful for running ad-hoc commands, monitoring status,
poking around and performing general maintenance on long-running
OpenCog or AtomSpace processes (e.g. robot control, large
batch-logicmoo processing or long-running data-mining servers.)

Having a network command line is notable: by default, Prolog
does not allow multiple users to access it at the same time.
As to scheme/guile, there is an ice-9 REPL server

The Logicmoo CogServer provides not only a convenient interface for ad-hoc
data hacking, but also provides a very good (and easy, and strong)
way of doing bulk data transfers. In particular, the transfer of
multiple megabytes of Atoms and (Truth)Values as UTF-8
(i.e. human-readable) text is easy and efficient. In particular,
it does not require fiddling with complex binary formats or
protocols or the use of protocol libraries or API's. (We're looking
at you, HTTP, REST, ZeroMQ, ProtoBuff and friends. You are all
very sophisticated, yes, but are hard to use. And sometimes painfully
slow.)

The logicmoo narative server is an
experimental prototype for controlling multiple threads and assigning
naratives processing priorities, in an AtomSpace-aware fashion. It is in
need of the caring love and attention from an interested developer.

For more info, please consult the
[logicmoo_cogserver wiki page](https://wiki.opencog.org/w/logicmoo_cogserver).

Using
-----
There are multiple ways to start the logicmoo_cogserver: from a bash shell prompt
(as a stand-alone process), from the guile command line, or from the
prolog command line.

* From bash, just start the process:
  `$ ./prolog/logicmoo_cogserver.pl`


* From prolog: `use_module(library(logicmoo_cogserver).` and then
   `?- start_cogserver().` (where's the documentation for this?)

Once started, one can obtain a shell by saying `rlwrap telnet localhost
12101`, and then `pl` or `scm` to obtain a prolog or scheme shell.  This
can be done as many times as desired; all shells share the same
AtomSpace, and the system is fully multi-threaded/thread-safe.

The `rlwrap` utility simply adds arrow-key support, so that up-arrow
provides a command history, and left-right arrow allows in-place editing.
Note that `telnet` does not provide any password protection!  It is
fully networked, so you can telnet from other hosts. The default port
number `21001` can be changed; see the documentation.

Building and Running
--------------------

To install the network of assertions, just follow the next sequence of
commands in your SWI-Prolog shell:

```bash
	$ swipl
	
	?- pack_install('https://github.com/opencog/logicmoo_cogserver.git').
	true.
```

OR 

```bash
	clone https://github.com/opencog/logicmoo_cogserver
	cd logicmoo_cogserver
	swipl
	?- pack_install('.').
	true.
```


Prerequisites
-------------
To run the logicmoo_cogserver, you need to install the SWI-Prolog first.
logicmoo_clif, Logicmoo's Common Logic Interchange Format

Unit tests
----------
To build and run the unit tests, just say
```
    cd test
```

Architecture
------------
See also these README's:

* [network/README](opencog/network/README.md)
* [logicmoo_cogserver/README](opencog/cogserver/server/README.md)
* [builtin-module/README](opencog/cogserver/modules/commands/README.md)
* [cython/README](opencog/cython/README.md)
* [agents-module/README](opencog/cogserver/modules/agents/README.md)
* [events-module/README](opencog/cogserver/modules/events/README.md)


## known issues

- Only `scm` shell is supported for now. There are plans to extend this support to other offered shells in the future.
- The logicmoo agent/logic system can be very hard to install and is optional from here

## licensing information

This package, like the most of OpenCog packages, is licensed under [AGPL-3.0 license](LICENSE).
