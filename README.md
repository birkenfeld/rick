# README for Rick

Rick is a Rust INTERCAL interpreter/compiler.

## Credits

Rick is modeled closely on the C-INTERCAL compiler written by Eric S. Raymond
and Alex Smith, and some algorithms for built-in functionality (operators,
binary I/O) is adapted from there, as well as the standard library in
`syslib.rs`.

Also, all the sample code from the `code` directory comes from the C-INTERCAL
distribution.  See the CATALOG file there for an overview of programs.

## Working principle

The INTERCAL interpreter takes a source file, parses it into an AST (abstract
sadist tree), and interprets the program, going through the statements.
