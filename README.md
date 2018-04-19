# scheme_interpreter

[![Build Status](https://travis-ci.org/rain-1/scheme_interpreter.svg?branch=master)](https://travis-ci.org/rain-1/scheme_interpreter)

The aims of this project are:

* Implement a mini scheme language useful for bootstrapping a self hosted scheme implementation.
* Have proper tail calls designed in from the beginning.
* Have a working GC, so we can process decent sized inputs.

# Usage

Make sure you get the tests submodule:

```
git submodule init
git submodule update
```

* `make` to build everything.
* `./util/run.sh <filename.scm>` to run a script file.
* `./util/repl.sh` to try out expressions in the REPL. Uses rlwrap.
