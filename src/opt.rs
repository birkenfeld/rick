// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal compiler.  Save your souls!
//
// Copyright (c) 2015-21 Georg Brandl
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

/// Optimizes INTERCAL code to look a little less like what your dog left on the carpet.
///
/// The optimizer gets the whole program and does several passes.
///
/// * constant folding: just reduces (sub)expressions involving no variables
/// * expressions: looks for common patterns of INTERCAL operator expressions
///   and replaces them by equivalent expressions involving native Rust operators
/// * constant output (can be disabled): if the program neither uses random numbers
///   nor takes any input, its output must be constant - the optimizer generates
///   this output using the Eval interpreter and replaces the program by a single
///   Print instruction (if your program does not terminate, you'd better disable
///   this pass with the -F option)
/// * abstain check: marks all statements that cannot be ABSTAINed from, so that
///   the code generator can skip emitting guards for them
/// * var check: marks all variables that cannot be IGNOREd, so that the code
///   generator can use unchecked assignments
///
/// The patterns recognized by the expression optimizer are pretty random.  They
/// were selected to optimize performance of the `tpk.i` example program, and
/// could be expanded a lot.  But at that point it's probably better to take the
/// route of C-INTERCAL and use a DSL for generic pattern matching.

use std::collections::BTreeMap;
use std::io::Cursor;
use std::u16;

use crate::ast::{Program, Stmt, StmtBody, Expr, Var, VarInfo, VType, Abstain};
use crate::eval;
use crate::stdops::{mingle, select, and_16, and_32, or_16, or_32, xor_16, xor_32};


pub struct Optimizer {
    program: Program,
    allow_const_out: bool,
}

fn n(i: u32) -> Box<Expr> {
    Box::new(Expr::Num(VType::I32, i))
}

impl Optimizer {
    pub fn new(program: Program, allow_const_out: bool) -> Optimizer {
        Optimizer { program, allow_const_out }
    }

    pub fn optimize(self) -> Program {
        let mut program = self.program;
        program = Optimizer::opt_constant_fold(program);
        program = Optimizer::opt_expressions(program);
        if self.allow_const_out {
            program = Optimizer::opt_const_output(program);
        }
        program = Optimizer::opt_abstain_check(program);
        program = Optimizer::opt_var_check(program);
        program
    }

    /// Fold expressions with literal constants, of which there are typically a lot
    /// since you can't have 32-bit literals.
    pub fn opt_constant_fold(mut program: Program) -> Program {
        for stmt in &mut program.stmts {
            match &mut stmt.body {
                StmtBody::Calc(_, expr) | StmtBody::Resume(expr) |
                StmtBody::Forget(expr) => Optimizer::fold(expr),
                _ => { }
            }
        }
        program
    }

    fn fold(expr: &mut Expr) {
        let mut result = None;
        match expr {
            Expr::Mingle(vx, wx) => {
                Optimizer::fold(vx);
                Optimizer::fold(wx);
                if let Expr::Num(_, v) = **vx {
                    if let Expr::Num(_, w) = **wx {
                        if v <= (u16::MAX as u32) &&
                           w <= (u16::MAX as u32) {
                            let z = mingle(v, w);
                            result = Some(*n(z));
                        }
                    }
                }
            }
            Expr::Select(_, vx, wx) => {
                Optimizer::fold(vx);
                Optimizer::fold(wx);
                if let Expr::Num(_, v) = **vx {
                    if let Expr::Num(_, w) = **wx {
                        let z = select(v, w);
                        result = Some(*n(z));
                    }
                }
            }
            Expr::And(_, vx) => {
                Optimizer::fold(vx);
                if let Expr::Num(vtype, v) = **vx {
                    result = Some(match vtype {
                        VType::I16 => Expr::Num(vtype, and_16(v)),
                        VType::I32 => Expr::Num(vtype, and_32(v)),
                    });
                }
            }
            Expr::Or(_, vx) => {
                Optimizer::fold(vx);
                if let Expr::Num(vtype, v) = **vx {
                    result = Some(match vtype {
                        VType::I16 => Expr::Num(vtype, or_16(v)),
                        VType::I32 => Expr::Num(vtype, or_32(v)),
                    });
                }
            }
            Expr::Xor(_, vx) => {
                Optimizer::fold(vx);
                if let Expr::Num(vtype, v) = **vx {
                    result = Some(match vtype {
                        VType::I16 => Expr::Num(vtype, xor_16(v)),
                        VType::I32 => Expr::Num(vtype, xor_32(v)),
                    });
                }
            }
            _ => {}
        }
        if let Some(result) = result {
            *expr = result;
        }
    }

    /// Optimize expressions.
    pub fn opt_expressions(mut program: Program) -> Program {
        for stmt in &mut program.stmts {
            //println!("\n\n{}", stmt.props.srcline);
            match &mut stmt.body {
                StmtBody::Calc(_, expr) | StmtBody::Resume(expr) |
                StmtBody::Forget(expr) => Optimizer::opt_expr(expr),
                _ => { }
            }
        }
        program
    }

    fn opt_expr(expr: &mut Expr) {
        //println!("optimizing {}", expr);
        let mut result = None;
        match expr {
            Expr::Select(_, vx, wx) => {
                Optimizer::opt_expr(vx);
                Optimizer::opt_expr(wx);
                match **wx {
                    // Select(UnOP(Mingle(x, y)), 0x5555_5555) = BinOP(x, y)
                    Expr::Num(_, 0x5555_5555) => {
                        match &**vx {
                            Expr::And(_, other) => {
                                if let Expr::Mingle(m1, m2) = &**other {
                                    result = Some(Expr::RsAnd(m1.clone(), m2.clone()));
                                }
                            }
                            Expr::Or(_, other) => {
                                if let Expr::Mingle(m1, m2) = &**other {
                                    result = Some(Expr::RsOr(m1.clone(), m2.clone()));
                                }
                            }
                            Expr::Xor(_, other) => {
                                if let Expr::Mingle(m1, m2) = &**other {
                                    result = Some(Expr::RsXor(m1.clone(), m2.clone()));
                                }
                            }
                            _ => { }
                        }
                    }
                    // Select(x, N) is a shift & mask if N has to "inside" zeros
                    // in binary notation
                    Expr::Num(_, i) if i.count_zeros() == i.leading_zeros() + i.trailing_zeros() => {
                        if i.trailing_zeros() == 0 {
                            result = Some(Expr::RsAnd(vx.clone(), n(i)));
                        } else if i.leading_zeros() == 0 {
                            result = Some(Expr::RsRshift(vx.clone(), n(i.trailing_zeros())));
                        } else {
                            result = Some(Expr::RsAnd(
                                Box::new(Expr::RsRshift(vx.clone(), n(i.trailing_zeros()))),
                                n((1 << i.count_ones()) - 1)));
                        }
                    }
                    // Select(Mingle(x, 0), 0x2AAA_AAAB)  ->  (x << 1) & 0xFFFF
                    Expr::Num(_, 0x2AAA_AAAB) => {
                        if let Expr::Mingle(m1, other) = &**vx {
                            if let Expr::Num(_, 0) = &**other {
                                result = Some(Expr::RsAnd(
                                    Box::new(Expr::RsLshift(m1.clone(), n(1))), n(0xFFFF)));
                            }
                        }
                    }
                    _ => { }
                }
            }
            Expr::Mingle(vx, wx) => {
                Optimizer::opt_expr(vx);
                Optimizer::opt_expr(wx);
                match (&**vx, &**wx) {
                    (Expr::RsAnd(vx1, vx2), Expr::RsAnd(wx1, wx2)) => {
                        match (&**vx1, &**vx2, &**wx1, &**wx2) {
                            // (x ~ 0xA..A) OP (y ~ 0xA..A) $ (x ~ 0x5..5) OP (y ~ 0x5..5)
                            // -> (x OP y) in 32-bit
                            (Expr::Select(_, ax, n1), Expr::Select(_, bx, n2),
                             Expr::Select(_, cx, n3), Expr::Select(_, dx, n4)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n2, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                matches!(**n4, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx && bx == dx =>
                                result = Some(Expr::RsAnd(ax.clone(), bx.clone())),
                            // (x ~ 0xA..A) OP y1 $ (x ~ 0x5..5) OP y2
                            // -> (x OP (y1 << 16 | y2)) in 32-bit
                            (Expr::Select(_, ax, n1), Expr::Num(_, bn),
                             Expr::Select(_, cx, n3), Expr::Num(_, dn)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx =>
                                result = Some(Expr::RsAnd(ax.clone(), n((bn << 16) | dn))),
                            _ => {}
                        }
                    }
                    (Expr::RsOr(vx1, vx2), Expr::RsOr(wx1, wx2)) => {
                        match (&**vx1, &**vx2, &**wx1, &**wx2) {
                            // as above
                            (Expr::Select(_, ax, n1), Expr::Select(_, bx, n2),
                             Expr::Select(_, cx, n3), Expr::Select(_, dx, n4)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n2, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                matches!(**n4, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx && bx == dx =>
                                result = Some(Expr::RsOr(ax.clone(), bx.clone())),
                            (Expr::Select(_, ax, n1), Expr::Num(_, bn),
                             Expr::Select(_, cx, n3), Expr::Num(_, dn)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx =>
                                result = Some(Expr::RsOr(ax.clone(), n((bn << 16) | dn))),
                            _ => {}
                        }
                    }
                    (Expr::RsXor(vx1, vx2), Expr::RsXor(wx1, wx2)) => {
                        match (&**vx1, &**vx2, &**wx1, &**wx2) {
                            (Expr::Select(_, ax, n1), Expr::Select(_, bx, n2),
                             Expr::Select(_, cx, n3), Expr::Select(_, dx, n4)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n2, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                matches!(**n4, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx && bx == dx =>
                                result = Some(Expr::RsXor(ax.clone(), bx.clone())),
                            (Expr::Select(_, ax, n1), Expr::Num(_, bn),
                             Expr::Select(_, cx, n3), Expr::Num(_, dn)) if
                                matches!(**n1, Expr::Num(_, 0xAAAA_AAAA)) &&
                                matches!(**n3, Expr::Num(_, 0x5555_5555)) &&
                                ax == cx =>
                                result = Some(Expr::RsXor(ax.clone(), n((bn << 16) | dn))),
                            _ => {}
                        }
                    }
                    // (x != y) $ (z != w)  ->  ((x != y) << 1) | (z != w)
                    (Expr::RsNotEqual(..), Expr::RsNotEqual(..)) =>
                        result = Some(Expr::RsOr(Box::new(Expr::RsLshift(vx.clone(), n(1))), wx.clone())),
                    _ => {}
                }
            }
            Expr::And(_, vx) | Expr::Or(_, vx) | Expr::Xor(_, vx) | Expr::RsNot(vx) => {
                Optimizer::opt_expr(vx);
            }
            Expr::RsAnd(vx, wx) => {
                Optimizer::opt_expr(vx);
                Optimizer::opt_expr(wx);
                match (&**vx, &**wx) {
                    // (x ~ x) & 1  ->  x != 0
                    (Expr::Select(_, sx, tx), Expr::Num(_, 1)) if sx == tx => {
                        result = Some(Expr::RsNotEqual(sx.clone(), n(0)));
                    }
                    (Expr::Xor(_, sx), Expr::Num(_, 3)) => {
                        if let Expr::Mingle(mx, n2) = &**sx {
                            // ?(x $ 1) & 3  ->  1 + (x & 1)
                            if matches!(**n2, Expr::Num(_, 1)) {
                                result = Some(Expr::RsPlus(n(1), Box::new(Expr::RsAnd(mx.clone(), n(1)))));
                            }
                            // ?(x $ 2) & 3  ->  2 - (x & 1)
                            if matches!(**n2, Expr::Num(_, 2)) {
                                result = Some(Expr::RsMinus(n(2), Box::new(Expr::RsAnd(mx.clone(), n(1)))));
                            }
                        }
                    }
                    // x & 0xFFFFFFFF has no effect
                    (_, Expr::Num(_, 0xFFFF_FFFF)) => {
                        result = Some(*vx.clone());
                    }
                    // Select(UnOP(Mingle(x, y)), 1) = BinOP(x & 1, y & 1)
                    (Expr::And(_, sx), Expr::Num(_, 1)) => {
                        if let Expr::Mingle(m1, m2) = &**sx {
                            result = Some(Expr::RsAnd(
                                Box::new(Expr::RsAnd(m1.clone(), n(1))),
                                Box::new(Expr::RsAnd(m2.clone(), n(1)))));
                        }
                    }
                    (Expr::Or(_, sx), Expr::Num(_, 1)) => {
                        if let Expr::Mingle(m1, m2) = &**sx {
                            result = Some(Expr::RsOr(
                                Box::new(Expr::RsAnd(m1.clone(), n(1))),
                                Box::new(Expr::RsAnd(m2.clone(), n(1)))));
                        }
                    }
                    (Expr::Xor(_, sx), Expr::Num(_, 1)) => {
                        if let Expr::Mingle(m1, m2) = &**sx {
                            result = Some(Expr::RsXor(
                                Box::new(Expr::RsAnd(m1.clone(), n(1))),
                                Box::new(Expr::RsAnd(m2.clone(), n(1)))));
                        }
                    }
                    // ((x & y) & y)  ->  second & has no effect
                    (Expr::RsAnd(_, v2x), _) if v2x == wx => {
                        result = Some(*vx.clone());
                    }
                    // ((x != y) & 1)  ->  & has no effect
                    (Expr::RsNotEqual(..), Expr::Num(_, 1)) => {
                        result = Some(*vx.clone());
                    }
                    _ => {}
                }
            }
            Expr::RsXor(vx, wx) => {
                Optimizer::opt_expr(vx);
                Optimizer::opt_expr(wx);
                if let Expr::Num(_, 0xFFFF_FFFF) = **wx {
                    result = Some(Expr::RsNot(vx.clone()));
                }
                else if let Expr::Num(_, 0xFFFF_FFFF) = **vx {
                    result = Some(Expr::RsNot(wx.clone()));
                }
            }
            // Expr::RsEqual(vx, wx) |
            Expr::RsOr(vx, wx) | Expr::RsRshift(vx, wx) | Expr::RsLshift(vx, wx) |
            Expr::RsNotEqual(vx, wx) | Expr::RsMinus(vx, wx) | Expr::RsPlus(vx, wx) => {
                Optimizer::opt_expr(vx);
                Optimizer::opt_expr(wx);
            }
            Expr::Num(..) | Expr::Var(..) => { }
        }
        if let Some(mut result) = result {
            Optimizer::opt_expr(&mut result);  // XXX will this always terminate?
            *expr = result;
        }
    }

    /// Cleverly check for programs that don't take input and always produce the
    /// same output; reduce them to a Print statement.
    pub fn opt_const_output(program: Program) -> Program {
        let mut possible = true;
        let mut prev_lbl = 0;
        for stmt in &program.stmts {
            // if we have a statement with %, no chance
            if stmt.props.chance < 100 {
                // except if it is one of the stdlibs itself
                if !(program.added_syslib && prev_lbl == 1901) &&
                    !(program.added_floatlib && (prev_lbl == 5401 || prev_lbl == 5402)) {
                        possible = false;
                        break;
                }
            }
            match stmt.body {
                // if we accept input, bail out
                StmtBody::WriteIn(..) => {
                    possible = false;
                    break;
                }
                // if we call one of the stdlib random routines, bail out
                StmtBody::DoNext(n) if ((n == 1900 || n == 1910 || n == 5400) &&
                                        prev_lbl != 1911) => {
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
        if eval::Eval::new(&program, &mut cursor, false, false).eval().is_err() {
            // if eval fails, don't pretend to do anything.
            return program;
        }
        Program {
            stmts: vec![Stmt::new_with(StmtBody::Print(cursor.into_inner())),
                        Stmt::new_with(StmtBody::GiveUp)],
            labels: BTreeMap::new(),
            stmt_types: vec![Abstain::Label(0)],
            var_info: (vec![], vec![], vec![], vec![]),
            uses_complex_comefrom: false,
            added_syslib: false,
            added_floatlib: false,
            bugline: 2
        }
    }

    /// Set "can_abstain" to false for all statements that can't be abstained from.
    pub fn opt_abstain_check(mut program: Program) -> Program {
        let mut can_abstain = vec![false; program.stmts.len()];
        for stmt in &program.stmts {
            match &stmt.body {
                StmtBody::Abstain(_, whats) |
                StmtBody::Reinstate(whats) => {
                    for what in whats {
                        if let Abstain::Label(lbl) = *what {
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
            if stmt.body != StmtBody::GiveUp {
                stmt.can_abstain = can_abstain;
            }
        }
        program
    }

    /// Determine "can_ignore" and "can_stash" for variables.
    pub fn opt_var_check(mut program: Program) -> Program {
        fn reset(vis: &mut Vec<VarInfo>) {
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
            match &stmt.body {
                StmtBody::Stash(vars) |
                StmtBody::Retrieve(vars) => {
                    for var in vars {
                        match *var {
                            Var::I16(n) => program.var_info.0[n].can_stash = true,
                            Var::I32(n) => program.var_info.1[n].can_stash = true,
                            Var::A16(n, _) => program.var_info.2[n].can_stash = true,
                            Var::A32(n, _) => program.var_info.3[n].can_stash = true,
                        }
                    }
                }
                StmtBody::Ignore(vars) | StmtBody::Remember(vars) => {
                    for var in vars {
                        match *var {
                            Var::I16(n) => program.var_info.0[n].can_ignore = true,
                            Var::I32(n) => program.var_info.1[n].can_ignore = true,
                            Var::A16(n, _) => program.var_info.2[n].can_ignore = true,
                            Var::A32(n, _) => program.var_info.3[n].can_ignore = true,
                        }
                    }
                }
                _ => { }
            }
        }
        program
    }
}
