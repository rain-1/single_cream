#!/bin/sh
cat src/init.scm $1 | valgrind --error-exitcode=1 ./bin/sch3
