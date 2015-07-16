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

use std::collections::HashMap;
use std::io::{ BufRead, stdin };
use std::u16;
use std::rc::Rc;

use ast;
use err;
use util::{ to_roman, from_english };


struct ArrayVar<T>(pub Vec<u16>, pub Vec<T>);

pub struct Eval {
    program: Rc<ast::Program>,
    spot: HashMap<u16, Vec<u16>>,
    twospot: HashMap<u16, Vec<u32>>,
    tail: HashMap<u16, Vec<ArrayVar<u16>>>,
    hybrid: HashMap<u16, Vec<ArrayVar<u32>>>,
    loglines: HashMap<u16, u16>,
    abstentions: Vec<bool>,
    stmt_types: Vec<ast::Abstain>,
    in_state: u8,
    out_state: u8,
}

enum StmtRes {
    Next,
    Jump(u16),
    End,
}

impl Eval {
    pub fn new(program: ast::Program) -> Eval {
        let mut abs = Vec::new();
        let mut types = Vec::new();
        let mut loglines = HashMap::new();
        for (i, stmt) in program.0.iter().enumerate() {
            abs.push(stmt.1.disabled);
            types.push(stmt.stype());
            if stmt.1.logline > 0 {
                loglines.insert(stmt.1.logline, i as u16);
            }
        }
        Eval {
            program: Rc::new(program),
            spot: HashMap::new(),
            twospot: HashMap::new(),
            tail: HashMap::new(),
            hybrid: HashMap::new(),
            loglines: loglines,
            abstentions: abs,
            stmt_types: types,
            in_state: 0,
            out_state: 0,
        }
    }

    pub fn eval(&mut self) -> Result<(), err::Error> {
        let mut pctr = 0u16;
        let program = self.program.clone();
        let nstmts = program.0.len() as u16;
        loop {
            if self.abstentions[pctr as usize] {
                pctr += 1;
            } else {
                let stmt = &program.0[pctr as usize];
                match try!(self.eval_stmt(stmt)) {
                    StmtRes::Next => pctr += 1,
                    StmtRes::Jump(n) => pctr = n,
                    StmtRes::End  => break,
                }
            }
            if pctr == nstmts {
                return Err(err::with_line(&err::IE663, program.0.len()));
            }
        }
        Ok(())
    }

    fn eval_stmt(&mut self, stmt: &ast::Stmt) -> Result<StmtRes, err::Error> {
        let line = stmt.1.srcline;
        match &stmt.0 {
            &ast::StmtType::GiveUp => Ok(StmtRes::End),
            &ast::StmtType::Error(ref e) => Err((*e).clone()),
            &ast::StmtType::DoNext(n) => {
                match self.loglines.get(&n) {
                    Some(i) => Ok(StmtRes::Jump(*i)),
                    None => {
                        Err(err::with_line(&err::IE129, line))
                    }
                }
            }
            &ast::StmtType::Calc(ref var, ref expr) => {
                let exval = try!(self.eval_expr(line, expr));
                try!(self.assign(line, var, exval));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::ReadOut(ref var) => {
                let varval = try!(self.lookup(line, var));
                println!("{}", to_roman(varval));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::WriteIn(ref var) => {
                let stdin = stdin();
                let mut slock = stdin.lock();
                let mut buf = String::new();
                let val = match slock.read_line(&mut buf) {
                    Ok(n) if n > 0 => try!(from_english(&buf)),
                    _              => return Err(err::with_line(&err::IE562, line))
                };
                try!(self.assign(line, var, val));
                Ok(StmtRes::Next)
            }
            _ => Err(err::with_line(&err::IE995, line))
        }
    }

    fn eval_expr(&self, line: usize, expr: &ast::Expr) -> Result<u32, err::Error> {
        match *expr {
            ast::Expr::Num(n) => Ok(n as u32),
            ast::Expr::Var(ref var) => self.lookup(line, var),
            _ => Err(err::with_line(&err::IE995, line)),
        }
    }

    fn assign(&mut self, line: usize, var: &ast::Var, val: u32) -> Result<(), err::Error> {
        match *var {
            ast::Var::I16(n) => {
                if val > u16::MAX as u32 {
                    Err(err::with_line(&err::IE275, line))
                } else {
                    let vec = self.spot.entry(n).or_insert(vec![]);
                    vec.pop();
                    vec.push(val as u16);
                    Ok(())
                }
            }
            ast::Var::I32(n) => {
                let vec = self.twospot.entry(n).or_insert(vec![]);
                vec.pop();
                vec.push(val);
                Ok(())
            }
            _ => Err(err::with_line(&err::IE995, line)),
        }
    }

    fn lookup(&self, line: usize, var: &ast::Var) -> Result<u32, err::Error> {
        match *var {
            ast::Var::I16(n) => {
                Ok(*self.spot.get(&n).and_then(|v| v.last()).unwrap_or(&0) as u32)
            }
            ast::Var::I32(n) => {
                Ok(*self.twospot.get(&n).and_then(|v| v.last()).unwrap_or(&0))
            }
            _ => Err(err::with_line(&err::IE995, line)),
        }
    }
}
