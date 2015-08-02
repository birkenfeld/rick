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

#![feature(plugin, iter_arith, box_syntax, box_patterns, append)]
#![plugin(rustlex)]

#[allow(plugin_as_library)]
extern crate rustlex;
extern crate rand;
extern crate time;

mod ast;
mod err;
mod lex;
mod parse;
mod eval;
mod util;
mod opt;
mod syslib;

use std::env::args;
use std::io::Read;
use std::fs::File;

use parse::Parser;
use opt::Optimizer;
use eval::Eval;


fn main() {

    let demo_prog = r##"
(1900)  DO STASH .2 + .3
        DO .3 <- #65535
        DO (1903) NEXT
(1903)  PLEASE FORGET #1
        DO .2 <- #0
        PLEASE %50 IGNORE .2
        DO .2 <- #1
        PLEASE REMEMBER .2
        DO .1 <- !1$.2'~"#65535$#1"
        DO .3 <- .3~#65534
        DO (1902) NEXT
        DO (1903) NEXT
(1902)  DO (1904) NEXT
        DO RETRIEVE .2 + .3
        DO FORGET #1
        PLEASE RESUME #1
(1904)  PLEASE RESUME '?"!3~.3'~#1"$#1'~#3
"##;

    let mut v;
    let argv = args().collect::<Vec<_>>();
    if argv.len() < 2 {
        v = demo_prog.as_bytes().to_vec();
    } else {
        let mut f = File::open(&argv[1]).unwrap();
        v = Vec::new();
        f.read_to_end(&mut v).unwrap();
    }

    let t0 = time::get_time();
    let program = match Parser::new(&v).parse() {
        Ok(program) => { println!("{}", program); program },
        Err(err)    => { println!("{}", err.to_string()); return },
    };

    let t1 = time::get_time();
    let program = Optimizer::new(program).optimize();
    println!("Optimized:\n\n{}", program);

    let t2 = time::get_time();
    if let Err(err) = Eval::new(program).eval() {
        println!("{}", err.to_string());
    }

    let t3 = time::get_time();
    println!("parsing:    {}", (t1 - t0));
    println!("optimizing: {}", (t2 - t1));
    println!("execution:  {}", (t3 - t2));
}
