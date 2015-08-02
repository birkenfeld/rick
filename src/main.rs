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

#![feature(plugin, box_syntax, box_patterns, append)]
#![plugin(rustlex)]
#![plugin(rick_syntex)]

#[allow(plugin_as_library)]
extern crate rustlex;
extern crate getopts;
extern crate time;

mod err;
mod lex;
mod parse;
mod ast;
mod opt;
mod eval;
mod codegen;
mod stdops;
mod syslib;

use std::env::args;
use std::io::Read;
use std::fs::{ File, remove_file };
use std::process::{ Command, Stdio };

use parse::Parser;
use opt::Optimizer;
use eval::Eval;
use codegen::Generator;


// XXX introduce E774
// XXX test suite
// XXX make user comfortable while rustc is running
// XXX syntax extensions:
// - computed come from
// - computed abstain (line number, #abstentions)
// - ONCE, AGAIN
fn main() {
    let args: Vec<String> = args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("i", "interpret", "interpret code instead of compiling");
    opts.optflag("d", "debug", "activate printing out debug messages");
    opts.optflag("c", "no-compile", "do not call rustc");
    opts.optflag("o", "opt", "optimize parsed code");
    opts.optflag("O", "rustc-opt", "run rustc in optimized mode");
    opts.optflag("h", "help", "print help message");

    // parse args
    let matches = match opts.parse(&args[1..]) {
        Ok(m)  => m,
        Err(e) => panic!(e.to_string()),
    };

    // handle help option
    if matches.opt_present("h") {
        println!("{}", opts.usage("rick [options] input.i"));
        return;
    }

    let compile_flag = !matches.opt_present("i");
    let debug_flag = matches.opt_present("d");
    let opt_flag = matches.opt_present("o");
    let rustc_flag = !matches.opt_present("c");
    let rustc_opt_flag = matches.opt_present("O");

    // no input file? -> do nothing
    if matches.free.is_empty() {
        return;
    }

    // verify and open input file
    let infile = &matches.free[0];
    if !infile.ends_with(".i") {
        print!("{}", err::IE998.new(None, 0).to_string());
        return;
    }
    let mut f = match File::open(&infile) {
        Err(_) => { print!("{}", err::IE777.new(None, 0).to_string()); return },
        Ok(f)  => f,
    };

    // read code from input file
    let mut code = Vec::new();
    if let Err(_) = f.read_to_end(&mut code) {
        print!("{}", err::IE777.new(None, 0).to_string());
        return;
    }

    // parse source
    let t0 = time::get_time();
    let mut program = match Parser::new(&code).get_program() {
        Ok(program) => {
            if debug_flag {
                println!("Parsed program:\n{}", program);
            }
            program
        }
        Err(err)    => { print!("{}", err.to_string()); return }
    };

    // optimize if wanted
    let t1 = time::get_time();
    if opt_flag {
        program = Optimizer::new(program).optimize();
    }

    // compile or run
    let t2 = time::get_time();
    if compile_flag {
        // PLEASE NOTE the selection of errors generated on different conditions
        // is a bit random
        let outname = String::from(&infile[..infile.len()-2]) + ".rs";
        // open output file
        let output = match File::create(&outname) {
            Err(_) => { print!("{}", err::IE888.new(None, 0).to_string()); return },
            Ok(f)  => f,
        };
        // generate Rust code
        match Generator::new(program, output, debug_flag).generate() {
            Err(err) => { print!("{}", err.to_string()); return },
            Ok(_)    => { }
        }
        let t3 = time::get_time();
        // if wanted, compile to binary
        if rustc_flag {
            let mut cmd = Command::new("rustc");
            if rustc_opt_flag {
                cmd.arg("-O");
            }
            cmd.arg("-o").arg(&outname[..outname.len()-3]);
            cmd.arg(&outname);
            cmd.stdout(Stdio::inherit()).stderr(Stdio::inherit());
            match cmd.output() {
                // remove intermediate .rs file on success
                Ok(ref s) if s.status.success() => {
                    let _ = remove_file(outname);
                },
                _ => {
                    print!("{}", err::IE666.new(None, 0).to_string());
                    return;
                }
            }
        }
        let t4 = time::get_time();
        if debug_flag {
            println!("parsing:    {}", (t1 - t0));
            println!("optimizing: {}", (t2 - t1));
            println!("code gen:   {}", (t3 - t2));
            println!("rustc:      {}", (t4 - t3));
        }
    } else {
        let num = match Eval::new(program, debug_flag).eval() {
            Err(err) => { print!("{}", err.to_string()); return },
            Ok(num)  => num,
        };
        let t3 = time::get_time();
        if debug_flag {
            println!("#stmts:     {}", num);
            println!("parsing:    {}", (t1 - t0));
            println!("optimizing: {}", (t2 - t1));
            println!("execute:    {}", (t3 - t2));
        }
    }
}
