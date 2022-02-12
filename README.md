LOGICMOO's AtomSpace Blackboard Server
===========================

opencog | singnet
------- | -------
xxxxxx  |  xxxxxx

The Logicmoo Cogserver is a network scheme/prolog command-line and logicmoo server for the [OpenCog framework](https://opencog.org).

The code in this git repo allows an AtomSpace to communicate with other AtomSpaces by having them all connect to a common CogServer.
The CogServer itself also provides an AtomSpace, which all clients interact with, in common.  In ASCII-art:
```
 +-------------+
 |  CogServer  |
 |    with     |  <-----internet------> Remote AtomSpace A
 |  AtomSpace  |  <---+
 +-------------+      |
                      +-- internet ---> Remote AtomSpace B

```

Here, AtomSpace A can load/store Atoms (and Values) to the CogServer,
as can AtomSpace B, and so these two can share AtomSpace contents
however desired.

This provides a simple, straight-forward backend for networking
together multiple AtomSpaces so that they can share data. This
backend, together with the file-based (RocksDB-based) backend
at [atomspace-rocks](https://github.com/opencog/atomspace-rocks)
is meant to provide a building-block out of which more complex
distributed and/or decentralized AtomSpaces can be built.

This really is decentralized: you can talk to multiple servers at once.
There is no particular limit, other than that of bandwidth,
response-time, etc.  In ASCII-art:

```
 +-----------+
 |           |  <---internet--> My AtomSpace
 |  Server A |                      ^  ^
 |           |        +-------------+  |
 +-----------+        v                v
                 +----------+   +-----------+
                 |          |   |           |
                 | Server B |   |  Server C |
                 |          |   |           |
                 +----------+   +-----------+
```

Prerequisites
-------------
To run the logicmoo_cogserver, you need to install the SWI-Prolog first.
logicmoo_clif, Logicmoo's Common Logic Interchange Format


# Installation

Using SWI-Prolog 7.1 or later:

```prolog
?- pack_install('https://github.com/logicmoo/logicmoo_cogserver.git').
true.
```


Using
-----
There are multiple ways to start the logicmoo_cogserver: from a bash shell prompt
(as a stand-alone process), from the guile command line, or from the
prolog command line.


* From bash, just start the process:
```bash
$ ./prolog/logicmoo_cogserver.pl`
``` 

* From prolog, load and start the process:
```prolog
?- use_module(library(logicmoo_cogserver)).
true.

?- start_cogserver().
true.

``` 

Once started, one can obtain a shell by saying `rlwrap telnet localhost
12101`, and then `pl` or `scm` to obtain a prolog or scheme shell.  This
can be done as many times as desired; all shells share the same
AtomSpace, and the system is fully multi-threaded/thread-safe.

The `rlwrap` utility simply adds arrow-key support, so that up-arrow
provides a command history, and left-right arrow allows in-place editing.
Note that `telnet` does not provide any password protection!  It is
fully networked, so you can telnet from other hosts. The default port
number `21001` can be changed; see the documentation.

- LOGICMOO Support Channel (Discord Invite Link)  https://discord.gg/JREW7F2
- LOGICMOO Telegram at [https://t.me/LogicMoo](https://t.me/LogicMoo)




## known issues

- Not all of `scm` shell is supported for now. There are plans to extend this support to other offered shells in the future.
- The logicmoo agent/logic system can be very hard to install and is optional from here
- Document this pack!
- Untangle the 'pack' install deps (Moving predicates over here from logicmoo_base)


## licensing information

This package, like the most of OpenCog packages, is licensed under [AGPL-3.0 license](LICENSE).



e  **StorageNode**  is a  [Node](https://wiki.opencog.org/w/Node "Node")  that provides basic infrastructure to exchange  [Atoms](https://wiki.opencog.org/w/Atom "Atom")  (using load, store, fetch, query) with other  [AtomSpaces](https://wiki.opencog.org/w/AtomSpace "AtomSpace")  and/or with conventional databases (for persistent (disk) storage). It provides interfaces for both database backends and for network  [distributed AtomSpaces](https://wiki.opencog.org/w/Distributed_AtomSpace "Distributed AtomSpace").

## Contents

-   [1  Implementations](https://wiki.opencog.org/w/StorageNode#Implementations)
    -   [1.1  Flat file API](https://wiki.opencog.org/w/StorageNode#Flat_file_API)
        -   [1.1.1  Socket API](https://wiki.opencog.org/w/StorageNode#Socket_API)
-   [2  Example](https://wiki.opencog.org/w/StorageNode#Example)
-   [3  The API](https://wiki.opencog.org/w/StorageNode#The_API)
-   [4  Creating new implementations](https://wiki.opencog.org/w/StorageNode#Creating_new_implementations)

## Implementations

Existing implementations include:

-   [RocksStorageNode](https://wiki.opencog.org/w/RocksStorageNode "RocksStorageNode")  -- Works with  [RocksDB](https://rocksdb.org/)  to save/restore AtomSpace data to the local filesystem.
-   [PostgresStorageNode](https://wiki.opencog.org/w/PostgresStorageNode "PostgresStorageNode")  -- Works with  [PostgreSQL](https://www.postgresql.org/)  to save/restore AtomSpace data to an SQL server.
-   [CogStorageNode](https://wiki.opencog.org/w/CogStorageNode "CogStorageNode")  -- Exchange atoms with another AtomSpace on the network, using the  [CogServer](https://wiki.opencog.org/w/CogServer "CogServer")  for communications.
-   [CogSimpleStorageNode](https://wiki.opencog.org/w/CogSimpleStorageNode "CogSimpleStorageNode")  -- Same as above, but provides a very simple, easy-to-understand example implementation. It can be used as a template for creating new types of StorageNodes.
-   [FileStorageNode](https://wiki.opencog.org/w/FileStorageNode "FileStorageNode")  -- Read/write plain-UTF8 (plain-ASCII) s-expressions to a flat file. This supports only a subset of the API suitable for flat files, so no search or query. Ideal for dumping and loading AtomSpaces as plain text.

All of these types use exactly the same API, described below.

Note that the StorageNode is a "private" (aka "abstract" or "pure virtual") type: it cannot be used, by itself; only the above subtypes can be directly created and used.

#### Flat file API

The flat-file API is designed to be high-performance and compact. It only supports a subset of the API below: one can write individual atoms, or dump the entire AtomSpace. It can load a complete file (all of it; subsections cannot be selected). There is no ability to search or query.

The main benefit of the flat-file API is that it is literally 10x faster than dumping atoms by hand to a file. Yes, we measured. Yes, the flat-file API has been performance tuned to go pretty much as fast as possible.

It is also ideal for backing up and archiving large AtomSpaces. When an Atom is stored as a plain-text s-expression, it is typically 50 to 250 bytes in size. When compressed (using bzip2) an Atom is 4 to 10 bytes in size. Thus even huge AtomSpaces have only modest sizes when dumped and compressed. Note that this is a lot smaller than the on-disk format for RocksDB: this is because RocksStorageNode maintains indexes to enable searching and queries. Those indexes take up a lot of storage! This is also about 1.3x to 4x smaller than dumping a Postgres database (of an AtomSpace) and compressing it. This is because the representation of an Atom in SQL is relatively complex.

See  [opencog/persist/sexpr](https://github.com/opencog/atomspace/tree/master/opencog/persist/sexpr)  for the implementation, and  [examples/atomspace/persist-store.scm](https://github.com/opencog/atomspace/tree/master/examples/atomspace/persist-store.scm)  for demo code.

##### Socket API

Note that if you are a clever programmer who understands TCP/IP sockets, then you can use the flat-file API to push Atomese content across a TCP/IP socket. Fast. However, if you do want to do this, you should take a very close look at the  [CogServer](https://wiki.opencog.org/w/CogServer "CogServer"). It has a custom high-speed mode, where it will accept and send Atomese across a socket, and it will also accept/send a certain subset of the commands below, just enough to implement the CogStorageNode. It's designed to be both super-fast and complete, doing everything you need to move Atoms around the network, as fast as possible.

## Example

Below is an example of loading an atom from RocksDB, and then sending it to two other CogServers. It is written in  [scheme](https://wiki.opencog.org/w/Scheme "Scheme"); you can do the same thing in  [python](https://wiki.opencog.org/w/Python "Python").

; Open connections
(define csna (CogStorageNode "cog://192.168.1.1"))
(define csnb (CogStorageNode "cog://192.168.1.2"))
(define rsn (RocksStorageNode "rocks:///tmp/foo.rdb")

; Load and send all [Values](https://wiki.opencog.org/w/Value "Value") attached to the Atom.
(fetch-atom ([Concept](https://wiki.opencog.org/w/Concept "Concept") "foo") rsn)
(store-atom (Concept "foo") csna)
(store-atom (Concept "foo") csnb)

; Specify a query
(define get-humans
   ([Meet](https://wiki.opencog.org/w/Meet "Meet") ([Inheritance](https://wiki.opencog.org/w/Inheritance "Inheritance") ([Variable](https://wiki.opencog.org/w/Variable "Variable") "$human") (Concept "person")))

; Specify a place where the results will be linked to 
(define results-key (Predicate "results"))

; Perform the query on the first cogserver 
(fetch-query get-humans results-key csna)

; Print the results to stdout
(cog-value get-humans results-key)

; Send the results to the second cogserver,
; and save locally to disk
(define results (cog-value get-humans results-key))
(store-atom results csnb)
(store-atom results rsn)

; Perform a clean shutdown
(cog-close csna)
(cog-close csnb)
(cog-close rsn)

A detailed, working version of the above can be found in github, in the  [AtomSpace persistence demo](https://github.com/opencog/atomspace/blob/master/examples/atomspace/persistence.scm)  and the  [persist-multi demo](https://github.com/opencog/atomspace/blob/master/examples/atomspace/persist-multi.scm). See also the other examples in the same directory.

Additional detailed, working examples can be found in the assorted github repos:

-   [CogStorage examples](https://github.com/opencog/atomspace-cog/tree/master/examples)
-   [RockStorage examples](https://github.com/opencog/atomspace-rocks/tree/master/examples)

## The API

There are fifteen functions for working with storage or a remote AtomSpace. These include opening and closing a connecting, sending and receiving individual Atoms, sending receiving them in bulk, and performing precise, selective queries to get only those Atoms you want.

The names given below are the  [scheme](https://wiki.opencog.org/w/Scheme "Scheme")  names for these functions; the equivalent C++ and  [python](https://wiki.opencog.org/w/Python "Python")  names are almost the same, using an underscore instead of a dash.

The methods are:

-   `cog-open`  -- Open a connection to the remote server or storage.
-   `cog-close`  -- Close the connection to the remote server/storage.
-   `fetch-atom`  -- fetch all of the  [Values](https://wiki.opencog.org/w/Value "Value")  on an  [Atom](https://wiki.opencog.org/w/Atom "Atom")  from the remote location.
-   `fetch-value`  -- fetch the single named Value on the given Atom. Handy if the Atom has lots of values on it, and you don't want to waste time getting all the others.
-   `store-atom`  -- store all of the Values on the given Atom. That is, send all of the Values to the remote location.
-   `store-value`  -- store just the single named Value on the given Atom. Handy if you don't want to change the other Values that the remote end is holding.
-   `cog-delete!`  -- Delete the Atom in the current AtomSpace, and also the attached storage. The Atom will  _not_  be deleted, if it has a non-empty incoming set.
-   `cog-delete-recursive!`  -- Delete the Atom in the current AtomSpace, and also the attached storage. If the Atom has a non-empty incoming set, then those Atoms will be deleted as well.
-   `load-atomspace`  -- get every Atom (and the attached Values) from the remote storage, and place them into the current AtomSpace.
-   `load-atoms-of-type`  -- get every Atom that is of the given type. This loads the Values on those Atoms as well.
-   `store-atomspace`  -- dump the entire contents of the current AtomSpace to the remote server/storage.
-   `fetch-incoming-set`  -- get all of the Atoms that are in the incoming set of the indicated Atom.
-   `fetch-incoming-by-type`  -- get all of the Atoms that are in the incoming set, and are also of the given type. Very useful when an Atom has a very large incoming set, and you want only a specific subset of it.
-   `fetch-query`  -- run the indicated query on the remote server/storage, and get all of the results of that query. You can use  [BindLink](https://wiki.opencog.org/w/BindLink "BindLink"),  [GetLink](https://wiki.opencog.org/w/GetLink "GetLink"),  [QueryLink](https://wiki.opencog.org/w/QueryLink "QueryLink"),  [MeetLink](https://wiki.opencog.org/w/MeetLink "MeetLink")  and  [JoinLink](https://wiki.opencog.org/w/JoinLink "JoinLink")  to formulate queries. Note that the query has a built-in caching mechanism, so that calling  `fetch-query`  a second time may return the same cached query results. That cache can be explicitly cleared, so that a second call re-runs the query. This is handy when queries are large and complex and consume lots of CPU time. There is an experimental time-stamp mechanism on the cache.
-   `barrier`  -- Force all prior network or storage operations to complete (on this particular StorageNode) before continuing with later operations.

Detailed and precise documentation can be displayed at the scheme and python command prompts. For example, typing  `,describe fetch-atom`  at the guile prompt will provide the man page for that function (note the comma in front of the "describe". You can shorten it to  `,d`). Something similar gets you the  [python](https://wiki.opencog.org/w/Python "Python")  documentation.

All of the above functions are provisional: they have been working and supported for quite a long time, but they should probably be replaced by pure  [Atomese](https://wiki.opencog.org/w/Atomese "Atomese"). That is, there should probably be a  `FetchAtomLink`  so that running  `([cog-execute!](https://wiki.opencog.org/w/Cog-execute! "Cog-execute!")  (FetchAtomLink ...))`  does exactly the same thing as calling  `fetch-atom`. This would be super-easy to do; it hasn't been done, because no one has asked for it yet.

The above API is implemented here:  [opencog/persist/api](https://github.com/opencog/atomspace/tree/master/opencog/persist/api).

## Creating new implementations

The  [CogSimpleStorageNode](https://wiki.opencog.org/w/CogSimpleStorageNode "CogSimpleStorageNode")  provides the simplest possible (more or less) working implementation of a StorageNode. It can be used as a guideline or example template, when creating new types of StorageNodes. To do this, simply copy the code located here:  [https://github.com/opencog/atomspace-cog/opencog/persist/cog-simple](https://github.com/opencog/atomspace-cog/opencog/persist/cog-simple)  and rename the files, gut the contents and replace them with what you need. Everything else, including the above-described API will work automatically. You will also need to add a new atom type to this file:  [opencog/persist/storage/storage_types.script](https://github.com/opencog/atomspace/tree/master/opencog/persist/storage/storage_types.script).

If you are designing a new interface to some database system, then it is strongly suggested that the code for the RocksStorageNode be copied and modified. Do  _**NOT**_  use the PostgresStorageNode as an example, even if you are contemplating another SQL database. The code written for PostgresStorageNode is very old, and is deeply flawed in many ways
