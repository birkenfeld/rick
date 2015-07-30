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

use std::u16;

use ast;
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
}
