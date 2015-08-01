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

use std::collections::BTreeMap;
use std::io::Cursor;
use std::u16;

use ast;
use eval;
use stdops::{ mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32 };


pub struct Optimizer {
    program: ast::Program,
}

impl Optimizer {
    pub fn new(program: ast::Program) -> Optimizer {
        Optimizer { program: program }
    }

    pub fn optimize(self) -> ast::Program {
        let program = self.program;
        let program = Optimizer::opt_constant_fold(program);
        let program = Optimizer::opt_const_output(program);
        let program = Optimizer::opt_abstain_check(program);
        let program = Optimizer::opt_var_check(program);
        program
    }

    /// Fold expressions with literal constants, of which there are typically a lot
    /// since you can't have 32-bit literals.
    pub fn opt_constant_fold(mut program: ast::Program) -> ast::Program {
        for stmt in &mut program.stmts {
            match stmt.body {
                ast::StmtBody::Calc(_, ref mut expr) => Optimizer::fold(expr),
                ast::StmtBody::Resume(ref mut expr)  => Optimizer::fold(expr),
                ast::StmtBody::Forget(ref mut expr)  => Optimizer::fold(expr),
                _ => { }
            }
        }
        program
    }

    fn fold(expr: &mut ast::Expr) {
        let mut result = None;
        match *expr {
            ast::Expr::Mingle(ref mut vx, ref mut wx) => {
                Optimizer::fold(vx);
                Optimizer::fold(wx);
                if let box ast::Expr::Num(_, v) = *vx {
                    if let box ast::Expr::Num(_, w) = *wx {
                        if v <= (u16::MAX as u32) &&
                           w <= (u16::MAX as u32) {
                            let z = mingle(v, w);
                            result = Some(ast::Expr::Num(ast::VType::I32, z));
                        }
                    }
                }
            }
            ast::Expr::Select(ref mut vx, ref mut wx) => {
                Optimizer::fold(vx);
                Optimizer::fold(wx);
                if let box ast::Expr::Num(_, v) = *vx {
                    if let box ast::Expr::Num(_, w) = *wx {
                        let z = select(v, w);
                        result = Some(ast::Expr::Num(ast::VType::I32, z));
                    }
                }
            }
            ast::Expr::And(_, ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(vtype, v) = *vx {
                    result = Some(match vtype {
                        ast::VType::I16 => ast::Expr::Num(vtype, and_16(v)),
                        ast::VType::I32 => ast::Expr::Num(vtype, and_32(v)),
                    });
                }
            }
            ast::Expr::Or(_, ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(vtype, v) = *vx {
                    result = Some(match vtype {
                        ast::VType::I16 => ast::Expr::Num(vtype, or_16(v)),
                        ast::VType::I32 => ast::Expr::Num(vtype, or_32(v)),
                    });
                }
            }
            ast::Expr::Xor(_, ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(vtype, v) = *vx {
                    result = Some(match vtype {
                        ast::VType::I16 => ast::Expr::Num(vtype, xor_16(v)),
                        ast::VType::I32 => ast::Expr::Num(vtype, xor_32(v)),
                    });
                }
            }
            _ => {}
        }
        if let Some(result) = result {
            *expr = result;
        }
    }

    /// Cleverly check for programs that don't take input and always produce the
    /// same output; reduce them to a Print statement.
    pub fn opt_const_output(program: ast::Program) -> ast::Program {
        let mut possible = true;
        let mut prev_lbl = 0;
        for stmt in &program.stmts {
            // if we have a statement with %, no chance
            if stmt.props.chance < 100 {
                // except if it is the syslib itself
                if !(program.added_syslib && prev_lbl == 1901) {
                    possible = false;
                    break;
                }
            }
            match stmt.body {
                // if we accept input, bail out
                ast::StmtBody::WriteIn(..) => {
                    possible = false;
                    break;
                }
                // if we call one of the syslib random routines, bail out
                ast::StmtBody::DoNext(n)
                    if (n == 1900 || n == 1910) && !(prev_lbl == 1911) => {
                    possible = false;
                    break;
                }
                _ => { }
            }
            prev_lbl = stmt.props.label;
        }
        if !possible {
            return program;
        }
        // we can do it! evaluate the program and replace all statements
        let out = Vec::new();
        let mut cursor = Cursor::new(out);
        if let Err(_) = eval::Eval::new(&program, &mut cursor, false).eval() {
            // if eval fails, don't pretend to do anything.
            return program;
        }
        let s = String::from_utf8(cursor.into_inner()).unwrap();
        ast::Program {
            stmts: vec![ast::Stmt::new_with(ast::StmtBody::Print(s)),
                        ast::Stmt::new_with(ast::StmtBody::GiveUp)],
            labels: BTreeMap::new(),
            stmt_types: vec![ast::Abstain::Label(0)],
            var_info: (vec![], vec![], vec![], vec![]),
            added_syslib: false
        }
    }

    /// Set "can_abstain" to false for all statements that can't be abstained from.
    pub fn opt_abstain_check(mut program: ast::Program) -> ast::Program {
        let mut can_abstain = vec![false; program.stmts.len()];
        for stmt in &program.stmts {
            match stmt.body {
                ast::StmtBody::Abstain(ref whats) |
                ast::StmtBody::Reinstate(ref whats) => {
                    for what in whats {
                        if let &ast::Abstain::Label(lbl) = what {
                            let idx = program.labels[&lbl];
                            can_abstain[idx as usize] = true;
                        } else {
                            for (i, stype) in program.stmt_types.iter().enumerate() {
                                if stype == what {
                                    can_abstain[i] = true;
                                }
                            }
                        }
                    }
                }
                _ => { }
            }
        }
        for (stmt, can_abstain) in program.stmts.iter_mut().zip(can_abstain) {
            if stmt.body != ast::StmtBody::GiveUp {
                stmt.can_abstain = can_abstain;
            }
        }
        program
    }

    /// Determine "can_ignore" and "can_stash" for variables.
    pub fn opt_var_check(mut program: ast::Program) -> ast::Program {
        fn reset(vis: &mut Vec<ast::VarInfo>) {
            for vi in vis {
                vi.can_stash = false;
                vi.can_ignore = false;
            }
        }
        reset(&mut program.var_info.0);
        reset(&mut program.var_info.1);
        reset(&mut program.var_info.2);
        reset(&mut program.var_info.3);
        for stmt in &program.stmts {
            match stmt.body {
                ast::StmtBody::Stash(ref vars) |
                ast::StmtBody::Retrieve(ref vars) => {
                    for var in vars {
                        match *var {
                            ast::Var::I16(n) => program.var_info.0[n].can_stash = true,
                            ast::Var::I32(n) => program.var_info.1[n].can_stash = true,
                            ast::Var::A16(n, _) => program.var_info.2[n].can_stash = true,
                            ast::Var::A32(n, _) => program.var_info.3[n].can_stash = true,
                        }
                    }
                }
                ast::StmtBody::Ignore(ref vars) |
                ast::StmtBody::Remember(ref vars) => {
                    for var in vars {
                        match *var {
                            ast::Var::I16(n) => program.var_info.0[n].can_ignore = true,
                            ast::Var::I32(n) => program.var_info.1[n].can_ignore = true,
                            ast::Var::A16(n, _) => program.var_info.2[n].can_ignore = true,
                            ast::Var::A32(n, _) => program.var_info.3[n].can_ignore = true,
                        }
                    }
                }
                _ => { }
            }
        }
        program
    }
}
