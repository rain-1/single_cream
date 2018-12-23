# scheme_interpreter

[![Build Status](https://travis-ci.org/rain-1/scheme_interpreter.svg?branch=master)](https://travis-ci.org/rain-1/scheme_interpreter)

The aims of this project are:

* Implement a mini scheme language useful for bootstrapping a self hosted scheme implementation.
* Have proper tail calls designed in from the beginning.
* Have a working GC, so we can process decent sized inputs.
* Basic macro system for syntactic extension.

# Usage

Make sure you get the tests submodule:

```
git submodule init
git submodule update
```

* `make` to build it.
* `make test` to run the test suite.
* `make analyze` to run analyzers (clang-analyze, valgrind)
* `make clean ; make CC=tcc` to build it with tinycc.
* `make clean ; make CC=clang CFLAGS='-fsanitize=address'`.
* `./util/run.sh <filename.scm>` to run a script file.
* `./util/run-valgrind.sh <filename.scm>` to run a script file with the interpreter being analyzed by valgrind.
* `./util/repl.sh` to try out expressions in the REPL. Uses rlwrap.

# Tests

It's very useful to have a comprehensive set of tests. Our tests are divided into suites, feel free to make new ones and import tests from other places.

* `t/trivial`: Extremely simple tests, checking that various objects can be READ and printed back.
* `t/simple`: Basic functionality tests for primitive functions. Also used to make sure simple recursive functions and such work before adding them to `init.scm`.
* `t/mal`: Taken some tests for the 'make a lisp' project.
* `t/rosetta`: Problems and solutions taken from the rosettacode code comparison site.
