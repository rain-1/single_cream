#!/bin/sh
cat src/init.scm src/preprocessor.scm src/std.scm $1 | ./bin/sch3

