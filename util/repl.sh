#!/bin/sh
(cat src/init.scm src/preprocessor.scm ; rlwrap cat) | ./bin/sch3

