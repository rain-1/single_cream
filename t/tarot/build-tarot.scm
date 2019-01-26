;;  sh ./t/tarot/cat.sh && ./util/run.sh ./t/tarot/tarot.scm
;;
;; git clone the tarot compiler to the single_cream directory
;; comment things in and out to compile a couple at time
;; update standard-functions.sch as you go
;;
;; yes this is a pest to do! sorry!

(load-macros)
;(compile-file "./tarot-compiler/std/cxr.scm")
(compile-file "./tarot-compiler/macros.scm")
;(compile-file "./tarot-compiler/std/boxes.scm")
;(compile-file "./tarot-compiler/std/numbers.scm")
;(compile-file "./tarot-compiler/std/equal.scm")
;(compile-file "./tarot-compiler/std/lists.scm")
;(compile-file "./tarot-compiler/std/display.scm")
;(compile-file "./tarot-compiler/std/queue.scm")
;(compile-file "./tarot-compiler/std/seq.scm")
;(compile-file "./tarot-compiler/std/shapes.scm")
;(compile-file "./tarot-compiler/std/stack.scm")
;(compile-file "./tarot-compiler/std/string.scm")
;(compile-file "./tarot-compiler/std/timer.scm")
;; cat ./tarot-compiler/std/*.sch >> standard-functions.sch
;(compile-file "./tarot-compiler/passes/parser.scm")
;(compile-file "./tarot-compiler/passes/desugar.scm")
;(compile-file "./tarot-compiler/passes/hoist.scm")
;(compile-file "./tarot-compiler/passes/denest.scm")
;(compile-file "./tarot-compiler/passes/flatten.scm")
;(compile-file "./tarot-compiler/passes/tmp-alloc.scm")
;(compile-file "./tarot-compiler/passes/assembler.scm")
;; cat ./tarot-compiler/passes/*.sch >> standard-functions.sch
;(compile-file "./tarot-compiler/compiler.scm")
;; cat tarot-compiler/compiler.sch >> standard-functions.sch
;(compile-file "./tarot-compiler/eval.scm")
;(compile-file "./tarot-compiler/compile-file.scm")
