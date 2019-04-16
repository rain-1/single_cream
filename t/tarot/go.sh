#!/bin/bash
set -e

T_TAROT_FILES="t/tarot/tarot-string.scm\
	      t/tarot/shapes.scm\
	      t/tarot/parser.scm\
	      t/tarot/assembler.scm\
	      t/tarot/denest.scm\
	      t/tarot/desugar.scm\
	      t/tarot/flatten.scm\
	      t/tarot/hoist.scm\
	      t/tarot/tmp-alloc.scm\
	      t/tarot/compiler.scm\
	      t/tarot/build-tarot.scm"

function update_standard_functions() {
	cat tarot-compiler/builtin-functions.scm > standard-functions.sch
	find "tarot-compiler/" -name "*.sch" -exec cat {} \; >> standard-functions.sch
}

function tarot_go() {
    echo building $@
    cat src/init.scm src/preprocessor.scm src/std.scm \
	$T_TAROT_FILES \
	| ./bin/sch3 -- $@
    update_standard_functions
}

# check if tarot-compiler/ exists, if not ask user to clone it

function chunk1() {

update_standard_functions
tarot_go ./tarot-compiler/std/cxr.scm
tarot_go ./tarot-compiler/macros.scm
tarot_go \
    "./tarot-compiler/std/numbers.scm" \
    "./tarot-compiler/std/equal.scm" \
    "./tarot-compiler/std/boxes.scm"
tarot_go \
    "./tarot-compiler/std/lists.scm"
tarot_go \
    "./tarot-compiler/std/display.scm"
tarot_go \
    "./tarot-compiler/std/queue.scm" \
    "./tarot-compiler/std/seq.scm" \
    "./tarot-compiler/std/shapes.scm" \
    "./tarot-compiler/std/stack.scm" \
    "./tarot-compiler/std/string.scm" \
    "./tarot-compiler/std/timer.scm"
}

function chunk2() {

tarot_go \
    "./tarot-compiler/passes/parser.scm"
tarot_go \
    "./tarot-compiler/passes/desugar.scm" \
    "./tarot-compiler/passes/hoist.scm" \
    "./tarot-compiler/passes/denest.scm" \
    "./tarot-compiler/passes/flatten.scm" \
    "./tarot-compiler/passes/tmp-alloc.scm" \
    "./tarot-compiler/passes/assembler.scm"
tarot_go \
    "./tarot-compiler/compiler.scm"
tarot_go \
    "./tarot-compiler/eval.scm" \
    "./tarot-compiler/compile-file.scm"
}

chunk1
chunk2
