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
use std::default::Default;
use std::fmt::{ Display, Error, Formatter };
use std::u16;

use err;


pub type Label = u16;
pub type LogLine = u16;

/// A whole program, with meta-information used at eval-time.
#[derive(PartialEq, Eq, Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub labels: HashMap<Label, LogLine>,
    pub comefroms: HashMap<Label, LogLine>,
    pub stmt_types: Vec<Abstain>,
    pub n_vars: (usize, usize, usize, usize),
}

/// A single statement.
#[derive(PartialEq, Eq, Debug)]
pub struct Stmt {
    pub body: StmtBody,
    pub props: StmtProps,
}

/// Common properties for all statements.
#[derive(PartialEq, Eq, Debug)]
pub struct StmtProps {
    pub label: Label,
    pub srcline: usize,
    pub chance: u8,
    pub polite: bool,
    pub disabled: bool,
}

/// Type-of-statement dependent data.
#[derive(PartialEq, Eq, Debug)]
pub enum StmtBody {
    Error(err::Error),
    Calc(Var, Expr),
    Dim(Var, Vec<Expr>),
    DoNext(Label),
    ComeFrom(Label),
    Resume(Expr),
    Forget(Expr),
    Ignore(Vec<Var>),
    Remember(Vec<Var>),
    Stash(Vec<Var>),
    Retrieve(Vec<Var>),
    Abstain(Abstain),
    Reinstate(Abstain),
    WriteIn(Var),
    ReadOut(Vec<Readout>),
    GiveUp,
}

/// An item in a READ OUT list.
#[derive(PartialEq, Eq, Debug)]
pub enum Readout {
    Var(Var),
    Const(u16),
}

/// A variable reference (store or load).
#[derive(PartialEq, Eq, Debug)]
pub enum Var {
    I16(usize),
    I32(usize),
    A16(usize, Vec<Expr>),
    A32(usize, Vec<Expr>),
}

/// An expression.
#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    Num(Val),
    Var(Var),
    Mingle(Box<Expr>, Box<Expr>),
    Select(Box<Expr>, Box<Expr>),
    And(Box<Expr>),
    Or(Box<Expr>),
    Xor(Box<Expr>),
}

/// An evaluated value.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Val {
    I16(u16),
    I32(u32),
}

/// Specification for an ABSTAIN or REINSTATE.
#[derive(PartialEq, Eq, Debug)]
pub enum Abstain {
    Line(Label),
    Calc,
    Next,
    Resume,
    Forget,
    Ignore,
    Remember,
    Stash,
    Retrieve,
    Abstain,
    Reinstate,
    ComeFrom,
    ReadOut,
    WriteIn,
}


impl Stmt {
    /// Determine the abstain type for the statement. Line(0) is used as an escape value.
    pub fn stype(&self) -> Abstain {
        match self.body {
            StmtBody::Error(_) => Abstain::Line(0),
            StmtBody::Calc(..) => Abstain::Calc,
            StmtBody::Dim(..) => Abstain::Calc,
            StmtBody::DoNext(_) => Abstain::Next,
            StmtBody::ComeFrom(_) => Abstain::ComeFrom,
            StmtBody::Resume(_) => Abstain::Resume,
            StmtBody::Forget(_) => Abstain::Forget,
            StmtBody::Ignore(_) => Abstain::Ignore,
            StmtBody::Remember(_) => Abstain::Remember,
            StmtBody::Stash(_) => Abstain::Stash,
            StmtBody::Retrieve(_) => Abstain::Retrieve,
            StmtBody::Abstain(_) => Abstain::Abstain,
            StmtBody::Reinstate(_) => Abstain::Reinstate,
            StmtBody::WriteIn(_) => Abstain::WriteIn,
            StmtBody::ReadOut(_) => Abstain::ReadOut,
            StmtBody::GiveUp => Abstain::Line(0),
        }
    }
}

impl StmtBody {
    // helpers for Display
    fn fmt_pluslist<T: Display>(&self, vars: &Vec<T>) -> String {
        vars.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(" + ")
    }

    fn fmt_bylist(&self, vars: &Vec<Expr>) -> String {
        vars.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().join(" BY ")
    }
}

impl Var {
    /// Is this Var a dimensioning access (array without subscript)?
    pub fn is_dim(&self) -> bool {
        match *self {
            Var::A16(_, ref v) if v.is_empty() => true,
            Var::A32(_, ref v) if v.is_empty() => true,
            _ => false,
        }
    }

    /// Get a unique key that identifies this variable among all var types.
    pub fn unique(&self) -> (u8, usize) {
        match *self {
            Var::I16(n)    => (0, n),
            Var::I32(n)    => (1, n),
            Var::A16(n, _) => (2, n),
            Var::A32(n, _) => (3, n),
        }
    }

    /// Rename the variable with a new number.
    pub fn rename(&mut self, new: usize) {
        match *self {
            Var::I16(ref mut n) |
            Var::I32(ref mut n) |
            Var::A16(ref mut n, _) |
            Var::A32(ref mut n, _) => {
                *n = new;
            }
        }
    }
}

impl Val {
    /// Cast as a 16-bit value; returns an Error if 32-bit and too big.
    pub fn as_u16(&self) -> Result<u16, err::Error> {
        match *self {
            Val::I16(v) => Ok(v),
            Val::I32(v) => {
                if v > (u16::MAX as u32) {
                    return Err(err::new(&err::IE275));
                }
                Ok(v as u16)
            }
        }
    }

    /// Cast as a 32-bit value; always succeeds.
    pub fn as_u32(&self) -> u32 {
        match *self {
            Val::I16(v) => v as u32,
            Val::I32(v) => v
        }
    }

    /// Create from a 32-bit value; will select the smallest possible type.
    pub fn from_u32(v: u32) -> Val {
        if v & 0xFFFF == v {
            Val::I16(v as u16)
        } else {
            Val::I32(v)
        }
    }
}


impl Default for StmtProps {
    fn default() -> StmtProps {
        StmtProps { label: 0,
                    srcline: 0,
                    chance: 100,
                    polite: false,
                    disabled: false, }
    }
}


impl Display for Program {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        for stmt in &self.stmts {
            try!(write!(fmt, "{}\n", stmt));
        }
        Ok(())
    }
}

impl Display for Stmt {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        try!(write!(fmt, "#{:03}  ", self.props.srcline));
        if self.props.label > 0 {
            try!(write!(fmt, "({:5}) ", self.props.label));
        } else {
            try!(write!(fmt, "        "));
        }
        if self.props.polite {
            try!(write!(fmt, "PLEASE "));
        } else {
            try!(write!(fmt, "DO     "));
        }
        if self.props.disabled {
            try!(write!(fmt, "NOT "));
        } else {
            try!(write!(fmt, "    "));
        }
        if self.props.chance < 100 {
            try!(write!(fmt, "%{} ", self.props.chance));
        }
        write!(fmt, "{}", self.body)
    }
}

impl Display for StmtBody {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            StmtBody::Error(ref err) => write!(fmt, "* {}", err.short_string()),
            StmtBody::Calc(ref var, ref expr) => write!(fmt, "{} <- {}", var, expr),
            StmtBody::Dim(ref var, ref exprs) => write!(fmt, "{} <- {}", var,
                                                        self.fmt_bylist(exprs)),
            StmtBody::DoNext(ref line) => write!(fmt, "({}) NEXT", line),
            StmtBody::ComeFrom(ref line) => write!(fmt, "COME FROM ({})", line),
            StmtBody::Resume(ref expr) => write!(fmt, "RESUME {}", expr),
            StmtBody::Forget(ref expr) => write!(fmt, "FORGET {}", expr),
            StmtBody::Ignore(ref vars) => write!(fmt, "IGNORE {}", self.fmt_pluslist(vars)),
            StmtBody::Remember(ref vars) => write!(fmt, "REMEMBER {}", self.fmt_pluslist(vars)),
            StmtBody::Stash(ref vars) => write!(fmt, "STASH {}", self.fmt_pluslist(vars)),
            StmtBody::Retrieve(ref vars) => write!(fmt, "RETRIEVE {}", self.fmt_pluslist(vars)),
            StmtBody::Abstain(ref what) => write!(fmt, "ABSTAIN FROM {}", what),
            StmtBody::Reinstate(ref what) => write!(fmt, "REINSTATE {}", what),
            StmtBody::WriteIn(ref var) => write!(fmt, "WRITE IN {}", var),
            StmtBody::ReadOut(ref vars) => write!(fmt, "READ OUT {}", self.fmt_pluslist(vars)),
            StmtBody::GiveUp => write!(fmt, "GIVE UP"),
        }
    }
}

impl Display for Readout {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Readout::Var(ref v) => write!(fmt, "{}", v),
            Readout::Const(n) => write!(fmt, "#{}", n),
        }
    }
}

impl Display for Var {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Var::I16(n) => write!(fmt, ".{}", n),
            Var::I32(n) => write!(fmt, ":{}", n),
            Var::A16(n, ref subs) => {
                try!(write!(fmt, ",{}", n));
                for sub in subs {
                    try!(write!(fmt, " SUB {}", sub));
                }
                Ok(())
            },
            Var::A32(n, ref subs) => {
                try!(write!(fmt, ";{}", n));
                for sub in subs {
                    try!(write!(fmt, " SUB {}", sub));
                }
                Ok(())
            },
        }
    }
}

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expr::Num(ref n) => write!(fmt, "{}", n),
            Expr::Var(ref v) => v.fmt(fmt),
            Expr::Mingle(ref x, ref y) => write!(fmt, "({})$({})", x, y),
            Expr::Select(ref x, ref y) => write!(fmt, "({})~({})", x, y),
            Expr::And(ref x) => write!(fmt, "&({})", x),
            Expr::Or(ref x) => write!(fmt, "V({})", x),
            Expr::Xor(ref x) => write!(fmt, "?({})", x),
        }
    }
}

impl Display for Val {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Val::I16(n) => write!(fmt, "#{}", n),
            Val::I32(n) => write!(fmt, "##{}", n),
        }
    }
}

impl Display for Abstain {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Abstain::Line(n) => write!(fmt, "({})", n),
            Abstain::Calc => write!(fmt, "CALCULATING"),
            Abstain::Next => write!(fmt, "NEXTING"),
            Abstain::Resume => write!(fmt, "RESUMING"),
            Abstain::Forget => write!(fmt, "FORGETTING"),
            Abstain::Ignore => write!(fmt, "IGNORING"),
            Abstain::Remember => write!(fmt, "REMEMBERING"),
            Abstain::Stash => write!(fmt, "STASHING"),
            Abstain::Retrieve => write!(fmt, "RETRIEVING"),
            Abstain::Abstain => write!(fmt, "ABSTAINING"),
            Abstain::Reinstate => write!(fmt, "REINSTATING"),
            Abstain::ComeFrom => write!(fmt, "COMING FROM"),
            Abstain::ReadOut => write!(fmt, "READING OUT"),
            Abstain::WriteIn => write!(fmt, "WRITING IN"),
        }
    }
}
