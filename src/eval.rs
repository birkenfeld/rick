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

use std::default::Default;
use std::rc::Rc;

use ast::{ self, Program, Stmt, StmtBody, Expr, Val, Var };
use err;
use util::{ write_number, write_byte, read_number, read_byte, check_chance,
            mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32 };


#[derive(Clone)]
struct Array<T> {
    pub dims: Vec<u16>,
    pub elems: Vec<T>,
}

impl<T: Clone + Default> Array<T> {
    pub fn new(dims: Vec<u16>) -> Array<T> {
        let total = dims.iter().product::<u16>() as usize;
        let value = Default::default();
        Array { dims: dims, elems: vec![value; total] }
    }

    pub fn empty() -> Array<T> {
        Array { dims: vec![], elems: vec![] }
    }
}

#[derive(Clone)]
struct Bind<T> {
    pub val: T,
    pub stack: Vec<T>,
    pub rw: bool,
}

impl<T> Bind<T> {
    pub fn new(t: T) -> Bind<T> {
        Bind { val: t, stack: Vec::new(), rw: true }
    }
}

pub struct Eval {
    program: Rc<Program>,
    spot: Vec<Bind<u16>>,
    twospot: Vec<Bind<u32>>,
    tail: Vec<Bind<Array<u16>>>,
    hybrid: Vec<Bind<Array<u32>>>,
    jumps: Vec<ast::LogLine>,
    abstentions: Vec<bool>,
    last_in: u8,
    last_out: u8,
}

type EvalRes<T> = Result<T, err::Error>;

enum StmtRes {
    Next,
    Jump(usize),
    Back(usize),
    End,
}

impl Eval {
    pub fn new(program: Program) -> Eval {
        let mut abs = Vec::new();
        for stmt in &program.stmts {
            abs.push(stmt.props.disabled);
        }
        let nv = program.n_vars;
        Eval {
            program: Rc::new(program),
            spot:    vec![Bind::new(0); nv.0],
            twospot: vec![Bind::new(0); nv.1],
            tail:    vec![Bind::new(Array::empty()); nv.2],
            hybrid:  vec![Bind::new(Array::empty()); nv.3],
            jumps:   Vec::new(),
            abstentions: abs,
            last_in: 0,
            last_out: 0,
        }
    }

    pub fn eval(&mut self) -> EvalRes<()> {
        let mut pctr = 0;  // index of current statement
        let program = self.program.clone();
        let nstmts = program.stmts.len();
        loop {
            // check for falling off the end
            if pctr >= nstmts {
                return Err(err::with_line(&err::IE663, nstmts));
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
                            continue;  // do not increment or check for COME FROMs
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
            if lbl > 0 {
                if let Some(next) = self.program.comefroms.get(&lbl) {
                    // check for abstained COME FROM
                    if !self.abstentions[*next as usize] {
                        pctr = *next as usize;
                        continue;
                    }
                }
            }
            // no COME FROM, normal execution
            pctr += 1;
        }
        Ok(())
    }

    /// Process a single statement.
    fn eval_stmt(&mut self, stmt: &Stmt) -> EvalRes<StmtRes> {
        //println!("        {}", stmt);
        match &stmt.body {
            &StmtBody::GiveUp => Ok(StmtRes::End),
            &StmtBody::Error(ref e) => Err((*e).clone()),
            &StmtBody::Calc(ref var, ref expr) => {
                let val = try!(self.eval_expr(expr));
                try!(self.assign(var, val));
                Ok(StmtRes::Next)
            }
            &StmtBody::Dim(ref var, ref exprs) => {
                let vals = try!(self.eval_exprlist(exprs));
                try!(self.array_dim(var, vals));
                Ok(StmtRes::Next)
            }
            &StmtBody::DoNext(n) => {
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
            &StmtBody::ComeFrom(_) => {
                // nothing to do here at runtime
                Ok(StmtRes::Next)
            }
            &StmtBody::Resume(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                let next = try!(self.pop_jumps(n, true)).unwrap();
                Ok(StmtRes::Back(next as usize))
            }
            &StmtBody::Forget(ref expr) => {
                let n = try!(self.eval_expr(expr)).as_u32();
                try!(self.pop_jumps(n, false));
                Ok(StmtRes::Next)
            }
            &StmtBody::Ignore(ref vars) => {
                for var in vars {
                    try!(self.set_rw(var, false));
                }
                Ok(StmtRes::Next)
            }
            &StmtBody::Remember(ref vars) => {
                for var in vars {
                    try!(self.set_rw(var, true));
                }
                Ok(StmtRes::Next)
            }
            &StmtBody::Stash(ref vars) => {
                for var in vars {
                    try!(self.stash(var));
                }
                Ok(StmtRes::Next)
            }
            &StmtBody::Retrieve(ref vars) => {
                for var in vars {
                    try!(self.retrieve(var));
                }
                Ok(StmtRes::Next)
            }
            &StmtBody::Abstain(ref what) => {
                try!(self.abstain(what, true));
                Ok(StmtRes::Next)
            }
            &StmtBody::Reinstate(ref what) => {
                try!(self.abstain(what, false));
                Ok(StmtRes::Next)
            }
            &StmtBody::ReadOut(ref vars) => {
                for var in vars {
                    match var {
                        &ast::Readout::Var(ref var) if var.is_dim() => {
                            try!(self.array_readout(var));
                        },
                        &ast::Readout::Var(ref var) => {
                            let varval = try!(self.lookup(var));
                            write_number(varval.as_u32());
                        },
                        &ast::Readout::Const(n) => write_number(n as u32),
                    };
                }
                Ok(StmtRes::Next)
            }
            &StmtBody::WriteIn(ref var) => {
                if var.is_dim() {
                    try!(self.array_writein(var));
                } else {
                    let n = try!(read_number());
                    try!(self.assign(var, Val::from_u32(n)));
                }
                Ok(StmtRes::Next)
            }
        }
    }

    /// Pop "n" jumps from the jump stack and return the last one.
    fn pop_jumps(&mut self, n: u32, strict: bool) -> EvalRes<Option<u16>> {
        if n == 0 {
            return Err(err::new(&err::IE621));
        }
        if self.jumps.len() < n as usize {
            if strict {
                return Err(err::new(&err::IE632));
            } else {
                self.jumps.clear();
                return Ok(None);
            }
        }
        let newlen = self.jumps.len() - (n as usize - 1);
        self.jumps.truncate(newlen);
        Ok(self.jumps.pop())
    }

    /// Evaluate an expression to a value.
    fn eval_expr(&self, expr: &Expr) -> EvalRes<Val> {
        match *expr {
            Expr::Num(ref n) => Ok(n.clone()),
            Expr::Var(ref var) => self.lookup(var),
            Expr::Mingle(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                mingle(v.as_u32(), w.as_u32()).map(Val::I32)
            },
            Expr::Select(ref vx, ref wx) => {
                let v = try!(self.eval_expr(vx));
                let w = try!(self.eval_expr(wx));
                select(v.as_u32(), w.as_u32()).map(Val::I32)
            },
            Expr::And(ref vx) => {
                match try!(self.eval_expr(vx)) {
                    Val::I16(v) => Ok(Val::I16(and_16(v))),
                    Val::I32(v) => Ok(Val::I32(and_32(v))),
                }
            },
            Expr::Or(ref vx) => {
                match try!(self.eval_expr(vx)) {
                    Val::I16(v) => Ok(Val::I16(or_16(v))),
                    Val::I32(v) => Ok(Val::I32(or_32(v))),
                }
            },
            Expr::Xor(ref vx) => {
                match try!(self.eval_expr(vx)) {
                    Val::I16(v) => Ok(Val::I16(xor_16(v))),
                    Val::I32(v) => Ok(Val::I32(xor_32(v))),
                }
            },
        }
    }

    fn eval_exprlist(&self, exprs: &Vec<Expr>) -> EvalRes<Vec<Val>> {
        exprs.iter().map(|v| self.eval_expr(v)).collect::<Result<Vec<_>, _>>()
    }

    /// Assign to a variable.
    fn assign(&mut self, var: &Var, val: Val) -> EvalRes<()> {
        //println!("assign: {:?} = {}", var, val.as_u32());
        match *var {
            Var::I16(n) => {
                let bind = &mut self.spot[n];
                if bind.rw {
                    bind.val = try!(val.as_u16());
                }
            },
            Var::I32(n) => {
                let bind = &mut self.twospot[n];
                if bind.rw {
                    bind.val = val.as_u32();
                }
            },
            Var::A16(n, ref subs) => {
                let subs = try!(self.eval_exprlist(subs));
                let bind = &mut self.tail[n];
                let ix = try!(Eval::array_get_index(bind, subs));
                if bind.rw {
                    let val = try!(val.as_u16());
                    bind.val.elems[ix] = val;
                }
            },
            Var::A32(n, ref subs) => {
                let subs = try!(self.eval_exprlist(subs));
                let bind = &mut self.hybrid[n];
                let ix = try!(Eval::array_get_index(bind, subs));
                if bind.rw {
                    bind.val.elems[ix] = val.as_u32();
                }
            },
        }
        Ok(())
    }

    /// Dimension an array.
    fn array_dim(&mut self, var: &Var, dims: Vec<Val>) -> EvalRes<()> {
        fn generic_dimension<T: Clone + Default>(bind: &mut Bind<Array<T>>,
                                                 dims: Vec<Val>) -> EvalRes<()> {
            let dims = try!(dims.iter().map(|v| v.as_u16()).collect::<Result<Vec<_>, _>>());
            if dims.iter().product::<u16>() == 0 {
                return Err(err::new(&err::IE240));
            }
            if bind.rw {
                bind.val = Array::new(dims);
            }
            Ok(())
        }
        match *var {
            Var::A16(n, _) => {
                generic_dimension(&mut self.tail[n], dims)
            },
            Var::A32(n, _) => {
                generic_dimension(&mut self.hybrid[n], dims)
            },
            _ => unimplemented!()
        }
    }

    /// Look up the value of a variable.
    fn lookup(&self, var: &Var) -> EvalRes<Val> {
        match *var {
            Var::I16(n) => {
                Ok(Val::I16(self.spot[n].val))
            },
            Var::I32(n) => {
                Ok(Val::I32(self.twospot[n].val))
            },
            Var::A16(n, ref subs) => {
                let subs = try!(self.eval_exprlist(subs));
                let bind = &self.tail[n];
                let ix = try!(Eval::array_get_index(bind, subs));
                Ok(Val::I16(bind.val.elems[ix]))
            },
            Var::A32(n, ref subs) => {
                let subs = try!(self.eval_exprlist(subs));
                let bind = &self.hybrid[n];
                let ix = try!(Eval::array_get_index(bind, subs));
                Ok(Val::I32(bind.val.elems[ix]))
            },
        }
    }

    /// Process a STASH statement.
    fn stash(&mut self, var: &Var) -> EvalRes<()> {
        fn generic_stash<T: Clone>(bind: &mut Bind<T>) {
            bind.stack.push(bind.val.clone());
        }
        match *var {
            Var::I16(n) => generic_stash(&mut self.spot[n]),
            Var::I32(n) => generic_stash(&mut self.twospot[n]),
            Var::A16(n, _) => generic_stash(&mut self.tail[n]),
            Var::A32(n, _) => generic_stash(&mut self.hybrid[n]),
        }
        Ok(())
    }

    /// Process a RETRIEVE statement.
    fn retrieve(&mut self, var: &Var) -> EvalRes<()> {
        fn generic_retrieve<T>(bind: &mut Bind<T>) -> EvalRes<()> {
            match bind.stack.pop() {
                None => Err(err::new(&err::IE436)),
                Some(v) => {
                    bind.val = v;
                    Ok(())
                }
            }
        }
        match *var {
            Var::I16(n) => generic_retrieve(&mut self.spot[n]),
            Var::I32(n) => generic_retrieve(&mut self.twospot[n]),
            Var::A16(n, _) => generic_retrieve(&mut self.tail[n]),
            Var::A32(n, _) => generic_retrieve(&mut self.hybrid[n]),
        }
    }

    /// Process an IGNORE or REMEMBER statement.
    fn set_rw(&mut self, var: &Var, rw: bool) -> EvalRes<()> {
        match *var {
            Var::I16(n) => self.spot[n].rw = rw,
            Var::I32(n) => self.twospot[n].rw = rw,
            Var::A16(n, _) => self.tail[n].rw = rw,
            Var::A32(n, _) => self.hybrid[n].rw = rw,
        }
        Ok(())
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

    /// Helper to calculate an array index.
    fn array_get_index<T>(bind: &Bind<Array<T>>, subs: Vec<Val>) -> EvalRes<usize> {
        let subs = try!(subs.iter().map(|v| v.as_u16()).collect::<Result<Vec<_>, _>>());
        if subs.len() != bind.val.dims.len() {
            return Err(err::new(&err::IE241));
        }
        let mut ix = 0;
        let mut prev_dim = 1;
        for (sub, dim) in subs.iter().zip(&bind.val.dims) {
            if *sub > *dim {
                return Err(err::new(&err::IE241));
            }
            ix += (sub - 1) * prev_dim;
            prev_dim = *dim;
        }
        Ok(ix as usize)
    }

    /// Array readout helper.
    fn array_readout(&mut self, var: &Var) -> EvalRes<()> {
        fn generic_readout<T: Copy, F>(bind: &Bind<Array<T>>, mut cb: F) -> EvalRes<()>
            where F: FnMut(T) -> ()
        {
            if bind.val.dims.len() != 1 {
                // only dimension-1 arrays can be output
                return Err(err::new(&err::IE241));
            }
            for val in bind.val.elems.iter() {
                cb(*val);
            }
            Ok(())
        }
        fn write(state: &mut u8, b: u16) {
            let byte = ((*state as i16 - b as i16) as u16 % 256) as u8;
            let mut c = byte;
            *state = byte as u8;
            c = (c & 0x0f) << 4 | (c & 0xf0) >> 4;
            c = (c & 0x33) << 2 | (c & 0xcc) >> 2;
            c = (c & 0x55) << 1 | (c & 0xaa) >> 1;
            write_byte(c);
        };
        let state = &mut self.last_out;
        match *var {
            Var::A16(n, _) => try!(generic_readout(&self.tail[n], |v| write(state, v))),
            Var::A32(n, _) => try!(generic_readout(&self.hybrid[n], |v| write(state, v as u16))),
            _ => unimplemented!()
        }
        Ok(())
    }

    /// Array writein helper.
    fn array_writein(&mut self, var: &Var) -> EvalRes<()> {
        fn generic_writein<T: Copy, F>(bind: &mut Bind<Array<T>>, mut cb: F) -> EvalRes<()>
            where F: FnMut() -> T
        {
            if bind.val.dims.len() != 1 {
                // only dimension-1 arrays can be input
                return Err(err::new(&err::IE241));
            }
            for place in bind.val.elems.iter_mut() {
                let b = cb();
                if bind.rw {
                    *place = b;
                }
            }
            Ok(())
        }
        fn read(state: &mut u8) -> u16 {
            let byte = read_byte();
            if byte == 256 {
                *state = 0;
                return 256;
            }
            let c = (byte as i16 - *state as i16) as u16 % 256;
            *state = byte as u8;
            c
        };
        let state = &mut self.last_in;
        match *var {
            Var::A16(n, _) => try!(generic_writein(&mut self.tail[n], || read(state))),
            Var::A32(n, _) => try!(generic_writein(&mut self.hybrid[n], || read(state) as u32)),
            _ => unimplemented!()
        }
        Ok(())
    }
}
