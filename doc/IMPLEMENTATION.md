# How are scheme values implemented

We use a tagged union called `struct Obj`.

# What is a root, and root stack

They root structs are elements in a doubly linked list that the garbage collector scans. It gets used for both `gc_roots` and `globals`. `globals` is an assoc list of all the top level definitions from scheme.

TODO: Can we actually just delete gc_roots?

The rootstack is also scanned by the garbage collector. It lets procedures make a few objects they want to work with locally "rooted" (in the sense that GC will not erase them), it's important to stick to the convention of popping off what you push onto the root stack.

# What is a semispace?

There are two semispaces. All the scheme objects live in one of them. Until it runs out of space. Then we copy everything that is alive over to the other. This is called a 2 space copy and compacting garbage collector. Specifically we use  Cheney's Algorithm which is described well here: https://en.wikipedia.org/wiki/Cheney%27s_algorithm

# What are gc forwarding pointers?

This is an internal detail of the garbage collector. GC means copying a whole complex directed graph of objects from one space to another. This can't all be done in one go, it needs to be done step by step leaving behind these forwarding pointers as markers, so that we know this part of the job was already done. A forwarding pointer will always be an object in the old space and it will always point into the new space.

# What's the symbol table about?

The symbol table implements `interning` which turns a short string of text (the symbol name) into a single integer. The main value of this is it lets you compare symbols for equality more efficiently and they often take up less memory. The symbol table is not garbage collected so you have to be a little bit economical with `gensym`.

# About the reader

Nothing much to say about the reader. Long form character names are not done yet.

# How are closures represented in this implementation?

A closure is a triple of args, env, body.

* `args` is the list of function parameters.
* `env` is the entire environment at the time the lambda was evaluated to produce the closure.
* `body` is the source code of the lambda body.

This representation is inspired by the following scheme code from the very standard kind of AST interpreter you might see:

```
(define (eval exp env)
  (cond ...
        ((lambda? exp)
	 (let ((args (lambda-args exp))
	       (body (lambda-body exp)))
	   (make-closure args env body)))
	...))

(define (apply clo vals)
   (eval (closure-body clo)
         (extend-env (zip-append (closure-args args) vals)
	             (closure-env clo))))         
```

# How does the interpreter work?

The idea behind the whole system was to take the standard kind of scheme AST based interpreter (parts of it sketched out above) you see, and simply translate that into C - in a single file - along with the runtime (object representation, GC, builtin functions) needed to make the thing fly.

So we have got various helper functions that the interpreter needs like `APPEND`, `ZIP-APPEND`, `ASSOC`, .. and then the full blown `EVAL` function - all written in C following the equivalent scheme code as closely as possible. I included the scheme code as comments in the C code to make the code as easy as possible follow. By nature the C versions are longer and involve some manual memory management.

# How are macros done?

(alternatively: what's `wrap_preprocess`)

The "REPL" (read eval print loop) actually has an extra step between `READ` and `EVAL`. After reading in an object it applies a `preprocess` function to it. This function is intitially just the identity function, but in `src/preprocess.scm` I redefine it (in scheme itself, rather than C) to a simple code walker expands out macros from a table.

The reason for this was to minimize the amount of C code we had to write - making the language self extensible in scheme helped a lot. The standard library `init.scm` is read before `preprocess.scm` so it can't use any of the extra language constructs it brings in.

The function itself just takes in the input object `EXP` and changes it to `(PREPROCESS (QUOTE EXP))`, then we evaluate that twice!

