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
use std::rc::Rc;
use std::u16;

use ast;
use err;
use util::{ write_number, read_number, check_chance,
            mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32 };


struct Array<T>(pub Vec<u16>, pub Vec<T>);
struct Var<T> {
    pub val: T,
    pub stack: Vec<T>,
    pub rw: bool,
}

impl<T> Var<T> {
    pub fn new(t: T) -> Var<T> {
        Var { val: t, stack: Vec::new(), rw: true }
    }
}

pub struct Eval {
    program: Rc<ast::Program>,
    spot: HashMap<u16, Var<u16>>,
    twospot: HashMap<u16, Var<u32>>,
    tail: HashMap<u16, Var<Array<u16>>>,
    hybrid: HashMap<u16, Var<Array<u32>>>,
    jumps: Vec<ast::LogLine>,
    abstentions: Vec<bool>,
    in_state: u8,
    out_state: u8,
}

type EvalRes<T> = Result<T, err::Error>;

enum StmtRes {
    Next,
    Jump(usize),
    Back(usize),
    End,
}

impl Eval {
    pub fn new(program: ast::Program) -> Eval {
        let mut abs = Vec::new();
        for stmt in &program.stmts {
            abs.push(stmt.props.disabled);
        }
        Eval {
            program: Rc::new(program),
            spot: HashMap::new(),
            twospot: HashMap::new(),
            tail: HashMap::new(),
            hybrid: HashMap::new(),
            jumps: Vec::new(),
            abstentions: abs,
            in_state: 0,
            out_state: 0,
        }
    }

    pub fn eval(&mut self) -> EvalRes<()> {
        let mut pctr = 0;  // index of current statement
        let program = self.program.clone();
        let nstmts = program.stmts.len();
        loop {
            // check for falling off the end
            if pctr >= nstmts {
                return Err(err::with_line(&err::IE663, nstmts as usize));
            }
            // execute statement if not abstained
            if !self.abstentions[pctr] {
                let stmt = &program.stmts[pctr];
                // check execution chance
                if check_chance(stmt.props.chance) {
                    let res = match self.eval_stmt(stmt) {
                        Err(mut err) => {
                            err.set_line(stmt.props.srcline);
                            return Err(err);
                        },
                        Ok(res)  => res
                    };
                    match res {
                        StmtRes::Jump(n) => {
                            self.jumps.push(pctr as u16);  // push the line with the NEXT
                            pctr = n;
                            continue;  // do not check for COME FROMs
                        },
                        StmtRes::Back(n) => {
                            pctr = n;  // will be incremented below after COME FROM check
                        }
                        StmtRes::End     => break,
                        StmtRes::Next    => { },
                    }
                }
            }
            // check for COME FROMs from this line
            let lbl = self.program.stmts[pctr].props.label;
            if let Some(next) = self.program.comefroms.get(&lbl) {
                // check for abstained COME FROM
                if !self.abstentions[*next as usize] {
                    pctr = *next as usize;
                    continue;
                }
            }
            // no COME FROM, normal execution
            pctr += 1;
        }
        Ok(())
    }

    /// Process a single statement.
    fn eval_stmt(&mut self, stmt: &ast::Stmt) -> EvalRes<StmtRes> {
        //println!("        {}", stmt);
        match &stmt.st {
            &ast::StmtType::GiveUp => Ok(StmtRes::End),
            &ast::StmtType::Error(ref e) => Err((*e).clone()),
            &ast::StmtType::Calc(ref var, ref expr) => {
                let val = try!(self.eval_expr(expr));
                try!(self.assign(var, val));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::DoNext(n) => {
                match self.program.labels.get(&n) {
                    Some(i) => {
                        if self.jumps.len() >= 80 {
                            return Err(err::new(&err::IE123))
                        }
                        Ok(StmtRes::Jump(*i as usize))
                    },
                    None => {
                        Err(err::new(&err::IE129))
                    }
                }
            }
            &ast::StmtType::ComeFrom(_) => {
                // nothing to do here at runtime
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Resume(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                let next = try!(self.pop_jumps(n));
                Ok(StmtRes::Back(next as usize))
            }
            &ast::StmtType::Forget(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                try!(self.pop_jumps(n));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Ignore(ref vars) => {
                for var in vars {
                    try!(self.set_rw(var, false));
                }
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Remember(ref vars) => {
                for var in vars {
                    try!(self.set_rw(var, true));
                }
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Stash(ref vars) => {
                for var in vars {
                    try!(self.stash(var));
                }
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Retrieve(ref vars) => {
                for var in vars {
                    try!(self.retrieve(var));
                }
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Abstain(ref what) => {
                try!(self.abstain(what, true));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::Reinstate(ref what) => {
                try!(self.abstain(what, false));
                Ok(StmtRes::Next)
            }
            &ast::StmtType::ReadOut(ref expr) => {
                let varval = try!(self.eval_expr(expr));
                write_number(varval.as_u32());
                Ok(StmtRes::Next)
            }
            &ast::StmtType::WriteIn(ref var) => {
                let n = try!(read_number());
                try!(self.assign(var, ast::Val::from_u32(n)));
                Ok(StmtRes::Next)
            }
        }
    }

    /// Evaluate an expression to a value.
    fn eval_expr(&self, expr: &ast::Expr) -> EvalRes<ast::Val> {
        match *expr {
            ast::Expr::Num(n) => Ok(ast::Val::I16(n)),
            ast::Expr::Var(ref var) => self.lookup(var),
            ast::Expr::Mingle(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                mingle(v.as_u32(), w.as_u32()).map(ast::Val::I32)
            },
            ast::Expr::Select(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                select(v.as_u32(), w.as_u32()).map(ast::Val::I32)
            },
            ast::Expr::And(ref vx) => {
                let v = try!(self.eval_expr(vx));
                match v {
                    ast::Val::I16(v) => Ok(ast::Val::I16(and_16(v))),
                    ast::Val::I32(v) => Ok(ast::Val::I32(and_32(v))),
                }
            }
            ast::Expr::Or(ref vx) => {
                let v = try!(self.eval_expr(vx));
                match v {
                    ast::Val::I16(v) => Ok(ast::Val::I16(or_16(v))),
                    ast::Val::I32(v) => Ok(ast::Val::I32(or_32(v))),
                }
            }
            ast::Expr::Xor(ref vx) => {
                let v = try!(self.eval_expr(vx));
                match v {
                    ast::Val::I16(v) => Ok(ast::Val::I16(xor_16(v))),
                    ast::Val::I32(v) => Ok(ast::Val::I32(xor_32(v))),
                }
            }
        }
    }

    /// Assign to a variable.
    fn assign(&mut self, var: &ast::Var, val: ast::Val) -> EvalRes<()> {
        println!("assign: {:?} = {}", var, val.as_u32());
        match *var {
            ast::Var::I16(n) => {
                let v = match val {
                    ast::Val::I16(v) => v,
                    ast::Val::I32(v) => {
                        if v > (u16::MAX as u32) {
                            return Err(err::new(&err::IE275));
                        }
                        v as u16
                    }
                };
                let vent = self.spot.entry(n).or_insert(Var::new(0));
                if vent.rw {
                    vent.val = v;
                }
                return Ok(());
            }
            ast::Var::I32(n) => {
                let vent = self.twospot.entry(n).or_insert(Var::new(0));
                if vent.rw {
                    vent.val = val.as_u32();
                }
                Ok(())
            }
            _ => Err(err::new(&err::IE995)),
        }
    }

    /// Pop "n" jumps from the jump stack and return the last one.
    fn pop_jumps(&mut self, n: u32) -> EvalRes<u16> {
        if n == 0 {
            return Err(err::new(&err::IE621));
        }
        if self.jumps.len() < n as usize {
            return Err(err::new(&err::IE632));
        }
        let newlen = self.jumps.len() - (n as usize - 1);
        self.jumps.truncate(newlen);
        Ok(self.jumps.pop().unwrap())
    }

    /// Look up the value of a variable.
    fn lookup(&self, var: &ast::Var) -> EvalRes<ast::Val> {
        match *var {
            ast::Var::I16(n) => {
                Ok(ast::Val::I16(self.spot.get(&n).map(|v| v.val).unwrap_or(0)))
            },
            ast::Var::I32(n) => {
                Ok(ast::Val::I32(self.twospot.get(&n).map(|v| v.val).unwrap_or(0)))
            },
            _ => Err(err::new(&err::IE995)),
        }
    }

    /// Process a STASH statement.
    fn stash(&mut self, var: &ast::Var) -> EvalRes<()> {
        match *var {
            ast::Var::I16(n) => {
                let vent = self.spot.entry(n).or_insert(Var::new(0));
                vent.stack.push(vent.val);
                Ok(())
            },
            ast::Var::I32(n) => {
                let vent = self.twospot.entry(n).or_insert(Var::new(0));
                vent.stack.push(vent.val);
                Ok(())
            },
            _ => Err(err::new(&err::IE995)),
        }
    }

    /// Process a RETRIEVE statement.
    fn retrieve(&mut self, var: &ast::Var) -> EvalRes<()> {
        match *var {
            ast::Var::I16(n) => {
                let vent = self.spot.entry(n).or_insert(Var::new(0));
                match vent.stack.pop() {
                    None => Err(err::new(&err::IE436)),
                    Some(v) => {
                        if vent.rw {
                            vent.val = v;
                        }
                        Ok(())
                    }
                }
            },
            ast::Var::I32(n) => {
                let vent = self.twospot.entry(n).or_insert(Var::new(0));
                match vent.stack.pop() {
                    None => Err(err::new(&err::IE436)),
                    Some(v) => {
                        if vent.rw {
                            vent.val = v;
                        }
                        Ok(())
                    }
                }
            },
            _ => Err(err::new(&err::IE995)),
        }
    }

    /// Process an IGNORE or REMEMBER statement.
    fn set_rw(&mut self, var: &ast::Var, rw: bool) -> EvalRes<()> {
        match *var {
            ast::Var::I16(n) => {
                let vent = self.spot.entry(n).or_insert(Var::new(0));
                vent.rw = rw;
                Ok(())
            },
            ast::Var::I32(n) => {
                let vent = self.twospot.entry(n).or_insert(Var::new(0));
                vent.rw = rw;
                Ok(())
            },
            _ => Err(err::new(&err::IE995)),
        }
    }

    /// Process an ABSTAIN or REINSTATE statement.
    fn abstain(&mut self, what: &ast::Abstain, abstain: bool) -> EvalRes<()> {
        if let &ast::Abstain::Line(n) = what {
            if !self.program.labels.contains_key(&n) {
                return Err(err::new(&err::IE139));
            }
            let idx = self.program.labels[&n];
            self.abstentions[idx as usize] = abstain;
        } else {
            for (i, stype) in self.program.stmt_types.iter().enumerate() {
                if stype == what {
                    self.abstentions[i] = abstain;
                }
            }
        }
        Ok(())
    }
}
