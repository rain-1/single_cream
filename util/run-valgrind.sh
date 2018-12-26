#!/bin/sh
cat src/init.scm src/preprocessor-2.scm $1 | valgrind --error-exitcode=1 ./bin/sch3
