# README for Rick

Rick is a Rust INTERCAL interpreter/compiler.

## Credits

Rick is modeled closely on the C-INTERCAL compiler written by Eric S. Raymond
and Alex Smith, and some algorithms for built-in functionality (operators,
binary I/O) is adapted from there, as well as the standard library in
`syslib.rs`.

Also, all the sample code from the `code` directory comes from the C-INTERCAL
distribution.  See the CATALOG file there for an overview of programs.

The idea of printing Mandelbrot while compiling is taken from PyPy.

## Language

A comprehensive INTERCAL resource page can be found at the C-INTERCAL page,
http://catb.org/esr/intercal/.

Rick implements the base INTERCAL-72 language with the following extensions:

* `COME FROM`
* Computed `COME FROM`
* `TRY AGAIN`
* Computed `ABSTAIN`
* Binary array I/O

## The interpreter

The INTERCAL interpreter takes a source file, parses it into an AST (abstract
sadist tree), and interprets the program, going through the statements.

## The compiler

Rick can also translate INTERCAL source to Rust, which is then compiled by the
system Rust compiler.  This is a few orders of magnitude slower than compiling
the C sources generated by C-INTERCAL, but achieves about the same runtime
performance, while being safe Rust code.

Rick itself uses unstable Rust features, but the generated code is stable-only.

One trick used for sharing code between the interpreter and programs translated
to Rust code is the syntax extension living in `rick_syntex`.  It contains an
attribute that will embed the decorated module's code as a string into the
module at compile time.  This is then written to the generated Rust files while
translating.

## Running

Run `cargo build` as usual.  Then you can run `cargo run -- --help` to see the
available options for the compiler.  Basic usage is `cargo run -- input.i` to
generate an executable and `cargo run -- -i input.i` to interpret.

You might want to use the `-b` flag to get rid of an annoying compiler bug (that
is mandated by the INTERCAL handbook).

Optimizations are activated with `-o` (optimizes INTERCAL code, recommended) and
`-O` (makes rustc optimize binary code, not recommended unless you bring lots of
time or the program is very small).  There are a few interesting optimizations,
such as folding the entire program to a "print" statement if it does not depend
on any input.

## Testing

The test suite consists of input and output files for the demo programs in
`code`.  Run `python test.py` to run the test suite.  Run `python test.py
--compiled` to also test compiled code, note however that this takes a while.
Use the `--short` flag to skip the most time consuming tests.
