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
use std::default::Default;
use std::fmt::{ Display, Error, Formatter };

use err::RtError;
use lex::SrcLine;

pub type Label = u16;
pub type LogLine = u16;

/// A whole program, with meta-information used at eval-time.
#[derive(PartialEq, Eq, Debug)]
pub struct Program {
    pub stmts: Vec<Stmt>,
    pub labels: BTreeMap<Label, LogLine>,
    pub stmt_types: Vec<Abstain>,
    pub var_info: (Vec<VarInfo>, Vec<VarInfo>, Vec<VarInfo>, Vec<VarInfo>),
    pub added_syslib: bool,
}

/// A single statement.
#[derive(PartialEq, Eq, Debug)]
pub struct Stmt {
    pub body: StmtBody,
    pub props: StmtProps,
    pub comefrom: Option<LogLine>,
    pub can_abstain: bool,
}

/// Common properties for all statements.
#[derive(PartialEq, Eq, Debug)]
pub struct StmtProps {
    pub label: Label,
    pub srcline: SrcLine,
    pub onthewayto: SrcLine,
    pub chance: u8,
    pub polite: bool,
    pub disabled: bool,
}

/// Type-of-statement dependent data.
#[derive(PartialEq, Eq, Debug)]
pub enum StmtBody {
    Error(RtError),
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
    Abstain(Vec<Abstain>),
    Reinstate(Vec<Abstain>),
    WriteIn(Vec<Var>),
    ReadOut(Vec<Expr>),
    GiveUp,
    // only used after optimizing
    Print(String),
}

/// A variable reference (store or load).
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Var {
    I16(usize),
    I32(usize),
    A16(usize, Vec<Expr>),
    A32(usize, Vec<Expr>),
}

/// An expression.
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Expr {
    Num(VType, u32),
    Var(Var),
    Mingle(Box<Expr>, Box<Expr>),
    Select(VType, Box<Expr>, Box<Expr>),
    And(VType, Box<Expr>),
    Or(VType, Box<Expr>),
    Xor(VType, Box<Expr>),
    // only used after optimizing
    RsNot(Box<Expr>),
    RsAnd(Box<Expr>, Box<Expr>),
    RsOr(Box<Expr>, Box<Expr>),
    RsXor(Box<Expr>, Box<Expr>),
    RsRshift(Box<Expr>, Box<Expr>),
    RsLshift(Box<Expr>, Box<Expr>),
    // RsEqual(Box<Expr>, Box<Expr>),
    RsNotEqual(Box<Expr>, Box<Expr>),
    RsPlus(Box<Expr>, Box<Expr>),
    RsMinus(Box<Expr>, Box<Expr>),
    // RsTimes(Box<Expr>, Box<Expr>),
    // RsDivide(Box<Expr>, Box<Expr>),
    // RsModulus(Box<Expr>, Box<Expr>),
}

/// Type of an expression.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum VType {
    I16,
    I32,
}

/// Specification for an ABSTAIN or REINSTATE.
#[derive(PartialEq, Eq, Debug)]
pub enum Abstain {
    Label(Label),
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

/// Information about a variable.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct VarInfo {
    pub can_ignore: bool,
    pub can_stash: bool,
}


impl Stmt {
    /// Determine the abstain type for the statement. Label(0) is used as an
    /// escape value.
    pub fn stype(&self) -> Abstain {
        match self.body {
            StmtBody::Error(_) => Abstain::Label(0),
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
            StmtBody::GiveUp => Abstain::Label(0),
            StmtBody::Print(_) => Abstain::Label(0),
        }
    }

    /// Synthesize a statement with default metadata.
    pub fn new_with(body: StmtBody) -> Stmt {
        Stmt { body: body, props: StmtProps::default(),
               comefrom: None, can_abstain: true }
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

impl Expr {
    pub fn get_vtype(&self) -> VType {
        match *self {
            Expr::Num(vtype, _) => vtype,
            Expr::And(vtype, _) | Expr::Or(vtype, _) | Expr::Xor(vtype, _) => vtype,
            Expr::Select(vtype, _, _) => vtype,
            Expr::Mingle(..) => VType::I32,
            Expr::RsAnd(..) | Expr::RsOr(..) | Expr::RsXor(..) |
            Expr::RsNot(..) | Expr::RsRshift(..) | Expr::RsLshift(..) |
            Expr::RsNotEqual(..) | Expr::RsMinus(..) |
            Expr::RsPlus(..) => VType::I32,
            Expr::Var(ref v) => v.get_vtype(),
        }
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

    /// Get the VType for this Var.
    pub fn get_vtype(&self) -> VType {
        match *self {
            Var::I16(..) | Var::A16(..) => VType::I16,
            Var::I32(..) | Var::A32(..) => VType::I32,
        }
    }
}

impl VarInfo {
    pub fn new() -> VarInfo {
        VarInfo { can_ignore: true, can_stash: true }
    }
}


impl Default for StmtProps {
    fn default() -> StmtProps {
        StmtProps { label: 0,
                    srcline: 0,
                    onthewayto: 0,
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
            StmtBody::Abstain(ref whats) => write!(fmt, "ABSTAIN FROM {}", self.fmt_pluslist(whats)),
            StmtBody::Reinstate(ref whats) => write!(fmt, "REINSTATE {}", self.fmt_pluslist(whats)),
            StmtBody::WriteIn(ref vars) => write!(fmt, "WRITE IN {}", self.fmt_pluslist(vars)),
            StmtBody::ReadOut(ref vars) => write!(fmt, "READ OUT {}", self.fmt_pluslist(vars)),
            StmtBody::GiveUp => write!(fmt, "GIVE UP"),
            StmtBody::Print(_) => write!(fmt, "<PRINT>"),
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
            }
            Var::A32(n, ref subs) => {
                try!(write!(fmt, ";{}", n));
                for sub in subs {
                    try!(write!(fmt, " SUB {}", sub));
                }
                Ok(())
            }
        }
    }
}

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Expr::Num(_, ref n) => write!(fmt, "#{:X}", n),
            Expr::Var(ref v) => v.fmt(fmt),
            Expr::Mingle(ref x, ref y) => write!(fmt, "({} $ {})", x, y),
            Expr::Select(_, ref x, ref y) => write!(fmt, "({} ~ {})", x, y),
            Expr::And(t, ref x) => write!(fmt, "&{} {}",
                                          if t == VType::I16 { "16" } else { "32" }, x),
            Expr::Or(t, ref x) => write!(fmt, "V{} {}",
                                         if t == VType::I16 { "16" } else { "32" }, x),
            Expr::Xor(t, ref x) => write!(fmt, "?{} {}",
                                          if t == VType::I16 { "16" } else { "32" }, x),
            // optimized exprs
            Expr::RsNot(ref x) => write!(fmt, "!{}", x),
            Expr::RsAnd(ref x, ref y) => write!(fmt, "({} & {})", x, y),
            Expr::RsOr(ref x, ref y) => write!(fmt, "({} | {})", x, y),
            Expr::RsXor(ref x, ref y) => write!(fmt, "({} ^ {})", x, y),
            Expr::RsRshift(ref x, ref y) => write!(fmt, "({} >> {})", x, y),
            Expr::RsLshift(ref x, ref y) => write!(fmt, "({} << {})", x, y),
            // Expr::RsEqual(ref x, ref y) => write!(fmt, "({} == {})", x, y),
            Expr::RsNotEqual(ref x, ref y) => write!(fmt, "({} != {})", x, y),
            Expr::RsPlus(ref x, ref y) => write!(fmt, "({} + {})", x, y),
            Expr::RsMinus(ref x, ref y) => write!(fmt, "({} - {})", x, y),
        }
    }
}

impl Display for Abstain {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Abstain::Label(n) => write!(fmt, "({})", n),
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
