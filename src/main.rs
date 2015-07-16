// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal interpreter.  Save your souls!
//
// Copyright (c) 2015 Georg Brandl
//
// This program is free software; you can redistribute it and/or modify it under the terms of the
// GNU General Public License as published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
// even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with this program;
// if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// -------------------------------------------------------------------------------------------------

#![feature(plugin)]
#![plugin(rustlex)]

#[allow(plugin_as_library)]
extern crate rustlex;

mod ast;
mod err;
mod lex;
mod parse;
mod eval;
mod util;


fn main() {
    use std::env::args;
    use std::io::Read;
    use std::fs::File;
    use parse::Parser;
    use eval::Eval;

//     let demo_prog = "
//         PLEASE DO ,1 <- #1
//         DO .4 <- #0
//         DO .5 <- #0
//         DO COME FROM (30)
//         DO WRITE IN ,1
//         DO .1 <- ,1SUB#1
//         DO (10) NEXT
//         PLEASE GIVE UP
//         PLEASE NOTE this is a comment
// (20)    PLEASE RESUME '?.1$#256'~'#256$#256'
// (10)    DO (20) NEXT
//         DO FORGET #1
//         PLEASE DO .2 <- .4
//         DO (1000) NEXT
//         DO .4 <- .3~#255
//         PLEASE DO .3 <- !3~#15'$!3~#240'
//         DO .3 <- !3~#15'$!3~#240'
//         DO .2 <- !3~#15'$!3~#240'
//         PLEASE DO .1 <- .5
//         DO (1010) NEXT
//         DO .5 <- .2
//         DO ,1SUB#1 <- .3
// (30)    PLEASE READ OUT ,1
// ";
    let demo_prog = "
PLEASE DO .1 <- #19999
PLEASE DO .2 <- .1
DO READ OUT .2
DO GIVE UP
";

    let mut v;
    let argv = args().collect::<Vec<_>>();
    if argv.len() < 2 {
        v = demo_prog.as_bytes().to_vec();
    } else {
        let mut f = File::open(&argv[1]).unwrap();
        v = Vec::new();
        f.read_to_end(&mut v).unwrap();
    }

    let program = match Parser::new(&v).parse() {
        Ok(program) => { println!("{}", program); program },
        Err(err)    => { println!("{}", err.to_string()); return },
    };

    if let Err(err) = Eval::new(program).eval() {
        println!("{}", err.to_string());
    }
}
