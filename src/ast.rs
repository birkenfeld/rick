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
use std::fmt::{ Display, Error, Formatter };

use err;


#[derive(PartialEq, Eq, Debug)]
pub struct Program(pub Vec<Stmt>);

#[derive(PartialEq, Eq, Debug)]
pub struct Stmt(pub StmtType, pub StmtProps);

#[derive(PartialEq, Eq, Debug)]
pub struct StmtProps {
    pub logline: u16,
    pub srcline: usize,
    pub chance: u8,
    pub polite: bool,
    pub disabled: bool,
}

#[derive(PartialEq, Eq, Debug)]
pub enum StmtType {
    Error(err::Error),
    Calc(Var, Expr),
    DoNext(u16),
    ComeFrom(u16),
    Resume(Expr),
    Forget(Expr),
    Stash(Vec<Var>),
    Retrieve(Vec<Var>),
    Abstain(Abstain),
    Reinstate(Abstain),
    WriteIn(Var),
    ReadOut(Var),
    GiveUp,
}

#[derive(PartialEq, Eq, Debug)]
pub enum Var {
    I16(u16),
    I32(u16),
    A16(u16, Vec<Expr>),
    A32(u16, Vec<Expr>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Expr {
    Num(u16),
    Var(Var),
    Mingle(Box<Expr>, Box<Expr>),
    Select(Box<Expr>, Box<Expr>),
    And(Box<Expr>),
    Or(Box<Expr>),
    Xor(Box<Expr>),
}

#[derive(PartialEq, Eq, Debug)]
pub enum Abstain {
    Line(u16),
    Calc,
    Next,
    Resume,
    Forget,
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
    pub fn stype(&self) -> Abstain {
        match self.0 {
            StmtType::Error(_) => Abstain::Line(0),
            StmtType::Calc(..) => Abstain::Calc,
            StmtType::DoNext(_) => Abstain::Next,
            StmtType::ComeFrom(_) => Abstain::ComeFrom,
            StmtType::Resume(_) => Abstain::Resume,
            StmtType::Forget(_) => Abstain::Forget,
            StmtType::Stash(_) => Abstain::Stash,
            StmtType::Retrieve(_) => Abstain::Retrieve,
            StmtType::Abstain(_) => Abstain::Abstain,
            StmtType::Reinstate(_) => Abstain::Reinstate,
            StmtType::WriteIn(_) => Abstain::WriteIn,
            StmtType::ReadOut(_) => Abstain::ReadOut,
            StmtType::GiveUp => Abstain::Line(0),
        }
    }
}


impl Default for StmtProps {
    fn default() -> StmtProps {
        StmtProps { logline: 0,
                    srcline: 0,
                    chance: 100,
                    polite: false,
                    disabled: false, }
    }
}


impl Display for Program {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        for stmt in &self.0 {
            try!(write!(fmt, "{}\n", stmt));
        }
        Ok(())
    }
}

impl Display for Stmt {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        if self.1.logline > 0 {
            try!(write!(fmt, "({:5}) ", self.1.logline));
        } else {
            try!(write!(fmt, "        "));
        }
        if self.1.polite {
            try!(write!(fmt, "PLEASE "));
        } else {
            try!(write!(fmt, "DO     "));
        }
        if self.1.disabled {
            try!(write!(fmt, "NOT "));
        } else {
            try!(write!(fmt, "    "));
        }
        if self.1.chance < 100 {
            try!(write!(fmt, "%{} ", self.1.chance));
        }
        write!(fmt, "{}", self.0)
    }
}

impl Display for StmtType {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            StmtType::Error(ref err) => write!(fmt, "* {}", err.short_string()),
            StmtType::Calc(ref var, ref expr) => write!(fmt, "{} <- {}", var, expr),
            StmtType::DoNext(ref line) => write!(fmt, "({}) NEXT", line),
            StmtType::ComeFrom(ref line) => write!(fmt, "COME FROM ({})", line),
            StmtType::Resume(ref expr) => write!(fmt, "RESUME {}", expr),
            StmtType::Forget(ref expr) => write!(fmt, "FORGET {}", expr),
            StmtType::Stash(ref vars) => write!(
                fmt, "STASH {}",
                vars.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().connect("+")),
            StmtType::Retrieve(ref vars) => write!(
                fmt, "RETRIEVE {}",
                vars.iter().map(|v| format!("{}", v)).collect::<Vec<_>>().connect("+")),
            StmtType::Abstain(ref what) => write!(fmt, "ABSTAIN FROM {}", what),
            StmtType::Reinstate(ref what) => write!(fmt, "REINSTATE {}", what),
            StmtType::WriteIn(ref var) => write!(fmt, "WRITE IN {}", var),
            StmtType::ReadOut(ref var) => write!(fmt, "READ OUT {}", var),
            StmtType::GiveUp => write!(fmt, "GIVE UP"),
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
            Expr::Num(n) => write!(fmt, "#{}", n),
            Expr::Var(ref v) => v.fmt(fmt),
            Expr::Mingle(ref x, ref y) => write!(fmt, "({})$({})", x, y),
            Expr::Select(ref x, ref y) => write!(fmt, "({})~({})", x, y),
            Expr::And(ref x) => write!(fmt, "&({})", x),
            Expr::Or(ref x) => write!(fmt, "V({})", x),
            Expr::Xor(ref x) => write!(fmt, "?({})", x),
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
