# single cream

[![Build Status](https://travis-ci.org/rain-1/single_cream.svg?branch=master)](https://travis-ci.org/rain-1/single_cream)

**single cream** is a single C file AST based interpreter for a subset of scheme. It includes a basic standard library in scheme and an unhygienic "defmacro".

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
* `make analyze` to run analyzers (clang-analyze, [cppcheck](https://github.com/danmar/cppcheck), valgrind)
* `./util/run.sh <filename.scm>` to run a script file.
* `./util/repl.sh` to try out expressions in the REPL. Uses rlwrap.
* `./util/run-valgrind.sh <filename.scm>` to run a script file with the interpreter being analyzed by valgrind.

For building and testing on nixos you can use the following shell:

```
nix-shell -p autoreconfHook -p clang -p clangAnalyzer -p valgrind -p tinycc -p rlwrap
````

## Testing

It's very useful to have a comprehensive set of tests. Our tests are divided into suites, feel free to make new ones and import tests from other places.

* `t/trivial`: Extremely simple tests, checking that various objects can be READ and printed back.
* `t/simple`: Basic functionality tests for primitive functions. Also used to make sure simple recursive functions and such work before adding them to `init.scm`.
* `t/mal`: Taken some tests for the 'make a lisp' project.
* `t/rosetta`: Problems and solutions taken from the rosettacode code comparison site.
* `t/sicp`: Ideas taken from the SICP book. nqueens is a particularly powerful stressor.

We have tested it with travis continuation integration, and on x64 linux and raspberry pi ARM linux.

## See also

* https://github.com/petermichaux/bootstrap-scheme
* https://github.com/rui314/minilisp
* https://github.com/edvb/tisp
