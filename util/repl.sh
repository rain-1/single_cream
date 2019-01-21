#!/bin/sh
(cat src/init.scm src/preprocessor.scm src/std.scm ; rlwrap cat) | ./bin/sch3

