# scheme_interpreter

The aims of this project are:

* Implement a mini scheme language useful for bootstrapping a self hosted scheme implementation.
* Have proper tail calls designed in from the beginning.
* Have a working GC, so we can process decent sized inputs.

The plan is to perform closure conversion on the input before interpreting it. We believe this makes closures much 'lighter', in the naive implementation closures hold the entire stack they were created in: stopping GC from collecting anything. The stack will also be implemented explicitly as a linked list of stack frames.

