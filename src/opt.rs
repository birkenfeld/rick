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

use ast;
use util::{ mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32 };



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
                if let box ast::Expr::Num(ref v) = *vx {
                    if let box ast::Expr::Num(ref w) = *wx {
                        if let Ok(z) = mingle(v.as_u32(), w.as_u32()) {
                            result = Some(ast::Expr::Num(ast::Val::I32(z)));
                        }
                    }
                }
            }
            ast::Expr::Select(ref mut vx, ref mut wx) => {
                Optimizer::fold(vx);
                Optimizer::fold(wx);
                if let box ast::Expr::Num(ref v) = *vx {
                    if let box ast::Expr::Num(ref w) = *wx {
                        if let Ok(z) = select(v.as_u32(), w.as_u32()) {
                            result = Some(ast::Expr::Num(ast::Val::I32(z)));
                        }
                    }
                }
            }
            ast::Expr::And(ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(ref v) = *vx {
                    result = Some(ast::Expr::Num(match v {
                        &ast::Val::I16(v) => ast::Val::I16(and_16(v)),
                        &ast::Val::I32(v) => ast::Val::I32(and_32(v)),
                    }));
                }
            }
            ast::Expr::Or(ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(ref v) = *vx {
                    result = Some(ast::Expr::Num(match v {
                        &ast::Val::I16(v) => ast::Val::I16(or_16(v)),
                        &ast::Val::I32(v) => ast::Val::I32(or_32(v)),
                    }));
                }
            }
            ast::Expr::Xor(ref mut vx) => {
                Optimizer::fold(vx);
                if let box ast::Expr::Num(ref v) = *vx {
                    result = Some(ast::Expr::Num(match v {
                        &ast::Val::I16(v) => ast::Val::I16(xor_16(v)),
                        &ast::Val::I32(v) => ast::Val::I32(xor_32(v)),
                    }));
                }
            }
            _ => {}
        }
        if let Some(result) = result {
            *expr = result;
        }
    }
}
