// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal compiler.  Save your souls!
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

#![feature(plugin, box_syntax, box_patterns, append, result_expect)]
#![plugin(rustlex)]

/// Main program for Rick.
///
/// Parses arguments, calls parser, optimizer, interpreter or code generator.

#[allow(plugin_as_library)]
extern crate rustlex;
extern crate getopts;
extern crate rand;
extern crate time;

macro_rules! stringify_passthrough {
    ($($t:item)*) => {
        pub const MODULE_CODE_STR: &'static str = stringify!($($t)*);
        $($t)*
    }
}

mod err;
mod lex;
mod parse;
mod ast;
mod opt;
mod eval;
mod codegen;
mod stdops;
mod syslib;
mod mandel;

use std::env::args;
use std::io::{ Read, Write, stdout, stderr };
use std::fs::{ File, remove_file };
use std::process::{ Command, Stdio, exit };
use std::sync::mpsc;
use std::thread;

use parse::Parser;
use opt::Optimizer;
use eval::Eval;
use codegen::Generator;


fn main() {
    match main_inner() {
        Ok(code) => exit(code),
        Err(err) => {
            let mut stderr = stderr();
            write!(stderr, "{}", err.to_string()).unwrap();
            exit(1);
        }
    }
}

fn main_inner() -> Result<i32, err::RtError> {
    let args: Vec<String> = args().collect();
    let mut opts = getopts::Options::new();
    opts.optflag("i", "interpret", "interpret code instead of compiling");
    opts.optflag("c", "no-compile", "do not call rustc");
    opts.optflag("o", "opt", "optimize parsed code");
    opts.optflag("b", "no-bug", "eliminate probability for E774");
    opts.optflag("O", "rustc-opt", "run rustc in optimized mode");
    opts.optflag("R", "no-random", "use deterministic random seed");
    opts.optflag("F", "no-constout", "do not optimize away const-output programs");
    opts.optflag("d", "debug", "activate printing out debug messages");
    opts.optflag("t", "timing", "print out timing messages");
    opts.optflag("h", "help", "print help message");

    // parse args
    let matches = match opts.parse(&args[1..]) {
        Ok(m)  => m,
        Err(e) => { println!("{}", e.to_string());
                    return err::IE990.err() },
    };

    // handle help option
    if matches.opt_present("h") {
        println!("{}", opts.usage("rick [options] input.i"));
        return Ok(0);
    }

    let compile_flag = !matches.opt_present("i");
    let debug_flag = matches.opt_present("d");
    let timing_flag = matches.opt_present("t");
    let opt_flag = matches.opt_present("o");
    let bug_flag = !matches.opt_present("b");
    let rand_flag = !matches.opt_present("R");
    let rustc_flag = !matches.opt_present("c");
    let rustc_opt_flag = matches.opt_present("O");
    let const_out_flag = !matches.opt_present("F");

    // no input file? -> do nothing
    if matches.free.is_empty() {
        return Ok(0);
    }

    // verify and open input file
    let infile = &matches.free[0];
    if !infile.ends_with(".i") {
        return err::IE998.err();
    }
    let mut f = match File::open(&infile) {
        Err(_) => return err::IE777.err(),
        Ok(f)  => f,
    };

    // read code from input file
    let mut code = Vec::new();
    if let Err(_) = f.read_to_end(&mut code) {
        return err::IE777.err();
    }

    // parse source
    let t0 = time::get_time();
    let mut program = match Parser::new(&code, 1, bug_flag).get_program() {
        Ok(program) => {
            if debug_flag {
                println!("Parsed program:\n{}", program);
            }
            program
        }
        Err(err)    => return Err(err),
    };

    // optimize if wanted
    let t1 = time::get_time();
    if opt_flag {
        program = Optimizer::new(program, const_out_flag).optimize();
        if debug_flag {
            println!("Optimized program:\n{}", program);
        }
    }

    // compile or run
    let t2 = time::get_time();
    if compile_flag {
        // PLEASE NOTE the selection of errors generated on different conditions
        // is a bit random
        let outname = String::from(&infile[..infile.len()-2]) + ".rs";
        // open output file
        let output = match File::create(&outname) {
            Err(_) => return err::IE888.err(),
            Ok(f)  => f,
        };
        // generate Rust code
        try!(Generator::new(program, output, debug_flag, rand_flag).generate());
        let t3 = time::get_time();
        // if wanted, compile to binary
        if rustc_flag {
            try!(run_compiler(&outname, rustc_opt_flag));
        }
        let t4 = time::get_time();
        if timing_flag {
            println!("parsing:    {}", (t1 - t0));
            println!("optimizing: {}", (t2 - t1));
            println!("code gen:   {}", (t3 - t2));
            println!("rustc:      {}", (t4 - t3));
        }
    } else {
        let mut stdout = stdout();
        if debug_flag {
            println!("Running:");
        }
        let mut eval = Eval::new(&program, &mut stdout, debug_flag, rand_flag);
        let num = try!(eval.eval());
        let t3 = time::get_time();
        if timing_flag {
            println!("#stmts:     {}", num);
            println!("parsing:    {}", (t1 - t0));
            println!("optimizing: {}", (t2 - t1));
            println!("execute:    {}", (t3 - t2));
        }
    }
    Ok(0)
}

fn run_compiler(outname: &str, opt_flag: bool) -> Result<(), err::RtError> {
    let mut cmd = Command::new("rustc");
    if opt_flag {
        cmd.arg("-O");
    }
    cmd.arg("-o").arg(&outname[..outname.len()-3]);
    cmd.arg(&outname);
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
    let child = match cmd.spawn() {
        Err(_) => return err::IE666.err(),
        Ok(ch) => ch,
    };
    // make the user comfortable...
    let (wchan, rchan) = mpsc::channel();
    let threadhandle = thread::spawn(move || {
        let mut printer = mandel::MandelPrinter::new();
        while let Err(mpsc::TryRecvError::Empty) = rchan.try_recv() {
            printer.print_char(true);
            thread::sleep_ms((2 as u32).pow((rand::random::<u8>() / 40) as u32));
        }
        printer.finish_current();
    });

    let wait_res = child.wait_with_output();
    wchan.send(()).unwrap();
    threadhandle.join().unwrap();
    let out = match wait_res {
        Err(_)  => return err::IE666.err(),
        Ok(out) => out,
    };
    print!("{}", String::from_utf8_lossy(&out.stderr));
    if !out.status.success() {
        return err::IE666.err();
    }
    let _ = remove_file(outname);
    Ok(())
}
