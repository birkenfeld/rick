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

use std::io::Write;

use ast::{ Program };
use err::{ Res, IE888 };

use err::MODULE_CODE_STR as ERR_MOD_STR;
use stdops::MODULE_CODE_STR as STDOPS_MOD_STR;


pub struct Generator<'a> {
    program: Program,
    output: &'a mut Write,
}

impl<'a> Generator<'a> {
    pub fn new(program: Program, output: &'a mut Write) -> Generator<'a> {
        Generator {
            program: program,
            output: output,
        }
    }

    fn write(&mut self, s: &str) -> Res<()> {
        match self.output.write(s.as_bytes()) {
            Ok(_)  => Ok(()),
            Err(_) => IE888.err(),
        }
    }

    pub fn generate(&mut self) -> Res<()> {
        try!(self.gen_stdmods());
        try!(self.gen_header());
        try!(self.gen_footer());
        Ok(())
    }

    fn gen_stdmods(&mut self) -> Res<()> {
        try!(self.write(ERR_MOD_STR));
        try!(self.write("\n\n"));
        try!(self.write(STDOPS_MOD_STR));
        try!(self.write("\n\n"));
        Ok(())
    }

    fn gen_header(&mut self) -> Res<()> {
        self.write("

extern crate rand;

fn main_inner() -> err::Res<()> {
")
    }

    fn gen_footer(&mut self) -> Res<()> {
        self.write("
}

fn main() {
    if let Err(err) = main_inner() {
        println!(\"{}\", err);
    }
}
")
    }
}
