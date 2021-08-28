// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal compiler.  Save your souls!
//
// Copyright (c) 2015-2021 Georg Brandl
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

/// Parses INTERCAL (yes, it's possible!) and generates an Abstract Sadism Tree.
///
/// Hand-written, since I didn't find any parser generators that looked nice enough.
/// The INTERCAL syntax is actually not that complicated (except where it is), but
/// the various extensions make it a bit iffy.
///
/// There are quite a few steps to do after parsing, which are done in the method
/// called `post_process`.  It makes a list of statements into a "real" program.

use std::collections::{BTreeMap, HashMap, hash_map::Entry as HEntry};
use std::io::{Read, BufRead, BufReader, Cursor};
use std::u16;
use std::str;
use itertools::Itertools;
use rand::{self, Rng};

use crate::ast::{self, Program, Stmt, StmtBody, StmtProps, Expr, Abstain, ComeFrom, Var, VType, VarInfo};
use crate::err::{Res, RtError, ErrDesc, IE000, IE017, IE079, IE099, IE139, IE182, IE197, IE200,
                 IE444, IE555, IE993};
use crate::lex::{SrcLine, SrcToken, Rule, Lexer, lex};
use crate::syslib;


#[derive(Debug)]
enum DecodeError {
    Hard(RtError),
    Soft(SrcLine),
}

type ParseRes<T> = Result<T, DecodeError>;

pub struct Parser<'p> {
    lines:  Vec<String>,
    tokens: Lexer<'p>,
    stash:  Vec<SrcToken>,  // used for backtracking
    startline: usize,
    allow_bug: bool,
}


impl<'p> Parser<'p> {
    pub fn new(code: &str, startline: usize, allow_bug: bool) -> Parser {
        // we have to keep a list of all physical source lines to generate
        // E000 error messages
        let cursor = Cursor::new(code);
        let lines = Parser::get_lines(BufReader::new(cursor));
        Parser { lines,
                 tokens: lex(code, startline),
                 stash: Vec::new(),
                 startline,
                 allow_bug }
    }

    fn get_lines<T: Read>(mut reader: BufReader<T>) -> Vec<String> {
        let mut buf = Vec::new();
        let mut res = Vec::new();
        while let Ok(n) = reader.read_until(b'\n', &mut buf) {
            if n == 0 {
                break;
            }
            let end = if buf[n - 1] == b'\n' { n - 1 } else { n };
            res.push(String::from_utf8_lossy(&buf[..end]).into_owned());
            buf.clear();
        }
        res
    }

    /// Parse the whole file as a program.
    pub fn get_program(&mut self) -> Res<Program> {
        // parse all statements
        let stmts = self.parse()?;
        // collect some necessary values and return the Program
        self.post_process(stmts)
    }

    pub fn parse(&mut self) -> Res<Vec<Stmt>> {
        // a program is a series of statements until EOF
        let mut stmts = Vec::new();
        loop {
            if self.tokens.peek().is_none() {
                break;
            }
            stmts.push(self.parse_stmt()?);
        }
        Ok(stmts)
    }

    /// Parse a single statement (correct or botched).
    fn parse_stmt(&mut self) -> Res<Stmt> {
        let mut props = StmtProps::default();
        // try to decode a statement
        self.stash.clear();
        match self.parse_stmt_maybe(&mut props) {
            // a hard error while parsing (rare)
            Err(DecodeError::Hard(err)) => Err(err),
            // a "soft" error: thrown at runtime as E000
            Err(DecodeError::Soft(srcline)) => {
                let body = StmtBody::Error(
                    IE000.mk(Some(self.lines[srcline - self.startline].clone()), 0));
                // jump over tokens until the next statement beginning
                loop {
                    match self.tokens.peek() {
                        None |
                        Some(Rule::DO) |
                        Some(Rule::PLEASEDO) => break,
                        Some(Rule::WAX) => {
                            let wax = self.tokens.next().expect("THERE WAX A TOKEN I SWEAR");
                            if let Some(Rule::NUMBER) = self.tokens.peek() {
                                self.tokens.push(wax);
                                break;
                            } else {
                                self.tokens.next();
                            }
                        }
                        _ => { self.tokens.next(); }
                    }
                }
                // return the botched statement
                Ok(Stmt { body, props, comefrom: None, can_abstain: true })
            }
            // a full statement!
            Ok(body) => {
                let can_abstain = body != StmtBody::GiveUp;
                Ok(Stmt { body, props, comefrom: None, can_abstain })
            }
        }
    }

    /// Try to parse a full statement.
    fn parse_stmt_maybe(&mut self, props: &mut StmtProps) -> ParseRes<StmtBody> {
        // parse logical line number label
        if let Some(label) = self.parse_label_maybe()? {
            props.label = label;
        }
        // parse statement inititiator
        if self.take(Rule::DO) {
            props.srcline = self.tokens.lineno();
        } else if self.take(Rule::PLEASEDO) {
            props.srcline = self.tokens.lineno();
            props.polite = true;
        } else {
            return Err(self.invalid());
        }
        // parse initial disable
        if self.take(Rule::NOT) {
            props.disabled = true;
        }
        // parse percentage
        if self.take(Rule::OHOHSEVEN) {
            let schance = self.req_number(100, &IE017)?;
            props.chance = schance as u8;
        }
        // parse statement meat
        // assignment?
        if let Some(var) = self.parse_var_maybe(true)? {
            self.req(Rule::GETS)?;
            if var.is_dim() {
                let exprs = self.parse_by_exprs()?;
                return Ok(StmtBody::Dim(var, exprs));
            } else {
                let expr = self.parse_expr()?;
                return Ok(StmtBody::Calc(var, expr));
            }
        }
        // next jump?
        if let Some(lbl) = self.parse_label_maybe()? {
            self.req(Rule::NEXT)?;
            return Ok(StmtBody::DoNext(lbl));
        }
        // other statements headed by keyword
        if self.take(Rule::COMEFROM) {
            if let Some(lbl) = self.parse_label_maybe()? {
                Ok(StmtBody::ComeFrom(ComeFrom::Label(lbl)))
            } else if let Ok(gerund) = self.parse_gerund() {
                Ok(StmtBody::ComeFrom(ComeFrom::Gerund(gerund)))
            } else {
                Ok(StmtBody::ComeFrom(ComeFrom::Expr(self.parse_expr()?)))
            }
        } else if self.take(Rule::RESUME) {
            Ok(StmtBody::Resume(self.parse_expr()?))
        } else if self.take(Rule::FORGET) {
            Ok(StmtBody::Forget(self.parse_expr()?))
        } else if self.take(Rule::IGNORE) {
            Ok(StmtBody::Ignore(self.parse_varlist(false)?))
        } else if self.take(Rule::REMEMBER) {
            Ok(StmtBody::Remember(self.parse_varlist(false)?))
        } else if self.take(Rule::STASH) {
            Ok(StmtBody::Stash(self.parse_varlist(false)?))
        } else if self.take(Rule::RETRIEVE) {
            Ok(StmtBody::Retrieve(self.parse_varlist(false)?))
        } else if self.take(Rule::ABSTAIN) {
            Ok(self.parse_abstain()?)
        } else if self.take(Rule::REINSTATE) {
            Ok(StmtBody::Reinstate(self.parse_abstain_items()?))
        } else if self.take(Rule::WRITEIN) {
            Ok(StmtBody::WriteIn(self.parse_varlist(true)?))
        } else if self.take(Rule::READOUT) {
            Ok(StmtBody::ReadOut(self.parse_readlist()?))
        } else if self.take(Rule::TRYAGAIN) {
            Ok(StmtBody::TryAgain)
        } else if self.take(Rule::GIVEUP) {
            Ok(StmtBody::GiveUp)
        } else {
            Err(self.invalid())
        }
    }

    /// Maybe parse a line label (N).
    fn parse_label_maybe(&mut self) -> ParseRes<Option<ast::Label>> {
        if self.take(Rule::WAX) {
            let lbl = self.req_number(u16::MAX, &IE197)?;
            self.req(Rule::WANE)?;
            Ok(Some(lbl))
        } else {
            Ok(None)
        }
    }

    /// Maybe parse a variable reference [.:,;]N {SUB X}.
    fn parse_var_maybe(&mut self, subs_allowed: bool) -> ParseRes<Option<Var>> {
        if self.take(Rule::SPOT) {
            let val = self.req_number(u16::MAX, &IE200)?;
            return Ok(Some(Var::I16(val.into())));
        }
        if self.take(Rule::TWOSPOT) {
            let val = self.req_number(u16::MAX, &IE200)?;
            return Ok(Some(Var::I32(val.into())));
        }
        if self.take(Rule::TAIL) {
            let val = self.req_number(u16::MAX, &IE200)?;
            let subs = if subs_allowed {
                self.parse_subs()?
            } else {
                vec![]
            };
            return Ok(Some(Var::A16(val.into(), subs)));
        }
        if self.take(Rule::HYBRID) {
            let val = self.req_number(u16::MAX, &IE200)?;
            let subs = if subs_allowed {
                self.parse_subs()?
            } else {
                vec![]
            };
            return Ok(Some(Var::A32(val.into(), subs)));
        }
        Ok(None)
    }

    /// Require a variable reference.
    fn parse_var(&mut self, subs_allowed: bool) -> ParseRes<Var> {
        self.parse_var_maybe(subs_allowed)?.ok_or_else(|| self.invalid())
    }

    /// Parse subscripts for a variable reference.
    fn parse_subs(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = Vec::new();
        if self.take(Rule::SUB) {
            // we need one expr at least
            res.push(self.parse_expr()?);
            // all others are optional; we need backtracking here
            loop {
                let state = self.stash.len();
                match self.parse_expr() {
                    Ok(expr) => res.push(expr),
                    Err(DecodeError::Soft(_)) => { self.backtrack(state); break }
                    Err(harderr) => return Err(harderr),
                }
            }
        }
        Ok(res)
    }

    /// Parse a list of variables separated by + (without subscripts).
    fn parse_varlist(&mut self, subs_allowed: bool) -> ParseRes<Vec<Var>> {
        let mut res = vec![self.parse_var(subs_allowed)?];
        while self.take(Rule::INTERSECTION) {
            res.push(self.parse_var(subs_allowed)?);
        }
        Ok(res)
    }

    /// Parse a list of variables (with subscripts) or consts separated by +.
    fn parse_readlist(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = Vec::new();
        if self.take(Rule::MESH) {
            let val = self.req_number(u16::MAX, &IE017)?;
            res.push(Expr::Num(VType::I16, val.into()));
        } else {
            res.push(Expr::Var(self.parse_var(true)?));
        }
        while self.take(Rule::INTERSECTION) {
            if self.take(Rule::MESH) {
                let val = self.req_number(u16::MAX, &IE017)?;
                res.push(Expr::Num(VType::I16, val.into()));
            } else {
                res.push(Expr::Var(self.parse_var(true)?));
            }
        }
        Ok(res)
    }

    /// Maybe parse a variable reference with maybe inline unary op [.:,;] OP N {SUB X}.
    fn parse_item_with_unop(&mut self) -> ParseRes<Option<Expr>> {
        fn parse_constr(self_: &mut Parser) -> Box<dyn Fn(Expr) -> Expr> {
            if self_.take(Rule::AMPERSAND) {
                Box::new(|e| Expr::And(VType::I16, Box::new(e)))
            } else if self_.take(Rule::BOOK) {
                Box::new(|e| Expr::Or(VType::I16, Box::new(e)))
            } else if self_.take(Rule::WHAT) {
                Box::new(|e| Expr::Xor(VType::I16, Box::new(e)))
            } else {
                Box::new(|e| e)
            }
        }
        if self.take(Rule::MESH) {
            let constr = parse_constr(self);
            let val = self.req_number(u16::MAX, &IE017)?;
            return Ok(Some(constr(Expr::Num(VType::I16, val.into()))));
        }
        if self.take(Rule::SPOT) {
            let constr = parse_constr(self);
            let val = self.req_number(u16::MAX, &IE200)?;
            return Ok(Some(constr(Expr::Var(Var::I16(val.into())))));
        }
        if self.take(Rule::TWOSPOT) {
            let constr = parse_constr(self);
            let val = self.req_number(u16::MAX, &IE200)?;
            return Ok(Some(constr(Expr::Var(Var::I32(val.into())))));
        }
        if self.take(Rule::TAIL) {
            let constr = parse_constr(self);
            let val = self.req_number(u16::MAX, &IE200)?;
            let subs = self.parse_subs()?;
            return Ok(Some(constr(Expr::Var(Var::A16(val.into(), subs)))));
        }
        if self.take(Rule::HYBRID) {
            let constr = parse_constr(self);
            let val = self.req_number(u16::MAX, &IE200)?;
            let subs = self.parse_subs()?;
            return Ok(Some(constr(Expr::Var(Var::A32(val.into(), subs)))));
        }
        Ok(None)
    }

    /// Parse an ABSTAIN statement.
    fn parse_abstain(&mut self) -> ParseRes<StmtBody> {
        let mut expr = None;
        if !self.take(Rule::FROM) {
            expr = Some(self.parse_expr()?);
            self.req(Rule::FROM)?;
        }
        Ok(StmtBody::Abstain(expr, self.parse_abstain_items()?))
    }

    /// Parse items following ABSTAIN FROM or REINSTATE.
    fn parse_abstain_items(&mut self) -> ParseRes<Vec<Abstain>> {
        // first form: a single line label
        if let Some(lbl) = self.parse_label_maybe()? {
            return Ok(vec![Abstain::Label(lbl)]);
        }
        // second form: one or more gerunds
        let mut res = vec![self.parse_gerund()?];
        while self.take(Rule::INTERSECTION) {
            res.push(self.parse_gerund()?);
        }
        Ok(res)
    }

    /// Parse a single gerund after ABSTAIN or REINSTATE.
    fn parse_gerund(&mut self) -> ParseRes<Abstain> {
        if self.take(Rule::CALCULATING) {
            Ok(Abstain::Calc)
        } else if self.take(Rule::NEXTING) {
            Ok(Abstain::Next)
        } else if self.take(Rule::RESUMING) {
            Ok(Abstain::Resume)
        } else if self.take(Rule::FORGETTING) {
            Ok(Abstain::Forget)
        } else if self.take(Rule::IGNORING) {
            Ok(Abstain::Ignore)
        } else if self.take(Rule::REMEMBERING) {
            Ok(Abstain::Remember)
        } else if self.take(Rule::STASHING) {
            Ok(Abstain::Stash)
        } else if self.take(Rule::RETRIEVING) {
            Ok(Abstain::Retrieve)
        } else if self.take(Rule::ABSTAINING) {
            Ok(Abstain::Abstain)
        } else if self.take(Rule::REINSTATING) {
            Ok(Abstain::Reinstate)
        } else if self.take(Rule::COMINGFROM) {
            Ok(Abstain::ComeFrom)
        } else if self.take(Rule::READINGOUT) {
            Ok(Abstain::ReadOut)
        } else if self.take(Rule::WRITINGIN) {
            Ok(Abstain::WriteIn)
        } else if self.take(Rule::TRYINGAGAIN) {
            Ok(Abstain::TryAgain)
        } else {
            Err(self.invalid())
        }
    }

    /// Parse a list of exprs separated by BY.
    fn parse_by_exprs(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = vec![self.parse_expr()?];
        while self.take(Rule::BY) {
            res.push(self.parse_expr()?);
        }
        Ok(res)
    }

    /// Parse a single expression.
    fn parse_expr(&mut self) -> ParseRes<Expr> {
        let left = self.parse_expr2()?;
        if self.take(Rule::MONEY) {
            let right = self.parse_expr2()?;
            return Ok(Expr::Mingle(Box::new(left), Box::new(right)));
        }
        if self.take(Rule::SQUIGGLE) {
            let right = self.parse_expr2()?;
            return Ok(Expr::Select(right.get_vtype(),
                                   Box::new(left), Box::new(right)));
        }
        Ok(left)
    }

    fn parse_expr2(&mut self) -> ParseRes<Expr> {
        if let Some(expr) = self.parse_item_with_unop()? {
            return Ok(expr);
        }
        if self.take(Rule::RABBITEARS) {
            let expr = self.parse_expr()?;
            self.req(Rule::RABBITEARS)?;
            Ok(expr)
        } else if self.take(Rule::SPARK) {
            let expr = self.parse_expr()?;
            self.req(Rule::SPARK)?;
            Ok(expr)
        } else if self.take(Rule::AMPERSAND) {
            let expr = self.parse_expr()?;
            Ok(Expr::And(expr.get_vtype(), Box::new(expr)))
        } else if self.take(Rule::BOOK) {
            let expr = self.parse_expr()?;
            Ok(Expr::Or(expr.get_vtype(), Box::new(expr)))
        } else if self.take(Rule::WHAT) {
            let expr = self.parse_expr()?;
            Ok(Expr::Xor(expr.get_vtype(), Box::new(expr)))
        } else {
            Err(self.invalid())
        }
    }

    /// If the next token is `t`, consume it and return true.
    #[inline]
    fn take(&mut self, t: Rule) -> bool {
        match self.tokens.peek() {
            Some(v) if v == t => { }
            _ => return false,
        }
        self.stash.push(self.tokens.next().expect("there just was a token?!"));
        true
    }

    /// Push tokens back to the source until stash is of length `state`.
    fn backtrack(&mut self, state: usize) {
        while self.stash.len() > state {
            self.tokens.push(self.stash.pop().expect("Schroedinger stack?"));
        }
    }

    /// Require a number as next token, with bounds checking.
    fn req_number(&mut self, max: u16, err: &'static ErrDesc) -> ParseRes<u16> {
        match self.tokens.next() {
            Some(t) => {
                if t.rule == Rule::NUMBER {
                    let x = t.value;
                    if x > max.into() {
                        Err(DecodeError::Hard(err.mk(None, self.tokens.lineno())))
                    } else {
                        self.stash.push(t);
                        Ok(x as u16)
                    }
                } else {
                    self.tokens.push(t);
                    Err(self.invalid())
                }
            }
            None => Err(self.invalid()),
        }
    }

    /// Require a token `r` next.
    fn req(&mut self, r: Rule) -> ParseRes<()> {
        match self.tokens.next() {
            None => Err(self.invalid()),
            Some(t) => {
                if t.rule == r {
                    self.stash.push(t);
                    Ok(())
                } else {
                    self.tokens.push(t);
                    Err(self.invalid())
                }
            }
        }
    }

    #[inline]
    fn invalid(&mut self) -> DecodeError {
        DecodeError::Soft(self.tokens.lineno())
    }

    /// Add the syslib to `stmts` if necessary.
    fn add_stdlibs(&self, mut stmts: Vec<Stmt>,
                   added_syslib: &mut bool, added_floatlib: &mut bool) -> Vec<Stmt> {
        let mut need_syslib = 0;
        let mut need_floatlib = 0;
        for stmt in &stmts {
            if stmt.props.label >= 1000 && stmt.props.label < 1999 {
                // we *are* the syslib or override its labels
                need_syslib = -1;
            }
            if stmt.props.label >= 5000 && stmt.props.label < 5999 {
                // we *are* the floatlib or override its labels
                need_floatlib = -1;
            }
            if let StmtBody::DoNext(n) = stmt.body {
                // jumping to a syslib label? we might need it then
                if (1000..1999).contains(&n) && need_syslib > -1 {
                    need_syslib = 1;
                }
                if (5000..5999).contains(&n) && need_floatlib > -1 {
                    need_floatlib = 1;
                }
            }
        }
        let mut last_lineno = self.tokens.lineno();
        if need_syslib == 1 {
            let mut p = Parser::new(syslib::SYSLIB_CODE, last_lineno, false);
            let mut syslib_stmts = p.parse().expect("E-42 SYSLIB BROKEN");
            stmts.append(&mut syslib_stmts);
            *added_syslib = true;
            last_lineno = p.tokens.lineno();
        }
        if need_floatlib == 1 {
            let mut p = Parser::new(syslib::FLOATLIB_CODE, last_lineno, false);
            let mut floatlib_stmts = p.parse().expect("E2.0000000001 FLOATLIB BROKEN");
            stmts.append(&mut floatlib_stmts);
            *added_floatlib = true;
        }
        stmts
    }

    /// Walk all references to variables, and call a visitor function for each.
    fn walk_vars<F>(&self, stmt: &mut Stmt, mut visitor: F)
        where F: FnMut(&mut Var)
    {
        let visitor = &mut visitor;

        fn walk_var<F>(var: &mut Var, visitor: &mut F)
            where F: FnMut(&mut Var)
        {
            visitor(var);
            match var {
                Var::A16(_, es) | Var::A32(_, es) => {
                    for e in es {
                        walk_expr(e, visitor);
                    }
                }
                Var::I16(_) | Var::I32(_) => { }
            }
        }

        fn walk_expr<F>(expr: &mut Expr, visitor: &mut F)
            where F: FnMut(&mut Var)
        {
            match expr {
                Expr::Var(v) => walk_var(v, visitor),
                Expr::And(_, e) |
                Expr::Or(_, e) |
                Expr::Xor(_, e) |
                Expr::RsNot(e) => walk_expr(e, visitor),
                Expr::Mingle(e, e2) |
                Expr::Select(_, e, e2) |
                Expr::RsAnd(e, e2) |
                Expr::RsOr(e, e2) |
                Expr::RsXor(e, e2) |
                Expr::RsRshift(e, e2) |
                Expr::RsLshift(e, e2) |
                // Expr::RsEqual(e, e2) |
                Expr::RsNotEqual(e, e2) |
                Expr::RsPlus(e, e2) |
                Expr::RsMinus(e, e2) => {
                    walk_expr(e, visitor);
                    walk_expr(e2, visitor);
                }
                Expr::Num(_, _) => { }
            }
        }

        match &mut stmt.body {
            StmtBody::Calc(v, e) => {
                walk_var(v, visitor);
                walk_expr(e, visitor);
            }
            StmtBody::Dim(v, es) => {
                walk_var(v, visitor);
                for e in es {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::Resume(e) |
            StmtBody::Forget(e) => {
                walk_expr(e, visitor);
            }
            StmtBody::Abstain(maybe_e, _) => {
                for e in maybe_e.iter_mut() {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::ComeFrom(spec) => {
                if let ComeFrom::Expr(e) = spec {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::Ignore(vs) |
            StmtBody::Remember(vs) |
            StmtBody::Stash(vs) |
            StmtBody::Retrieve(vs) |
            StmtBody::WriteIn(vs) => {
                for v in vs {
                    walk_var(v, visitor);
                }
            }
            StmtBody::ReadOut(es) => {
                for e in es {
                    walk_expr(e, visitor);
                }
            }
            _ => { }
        }
    }

    /// Collect all used variable numbers and renumber them.
    fn collect_vars(&self, vars: &mut Vars, stmt: &mut Stmt) {
        self.walk_vars(stmt, |var| {
            let key = var.unique();
            if let HEntry::Vacant(v) = vars.map.entry(key) {
                let idx = key.0 as usize;
                v.insert(vars.counts[idx]);
                vars.counts[idx] += 1;
            }
        });
    }

    /// Apply variable renumbering.
    fn rename_vars(&self, vars: &Vars, stmt: &mut Stmt) {
        self.walk_vars(stmt, |var| {
            let key = var.unique();
            var.rename(vars.map[&key]);
        });
    }

    /// Do whatever needs to be done after parsing is complete.
    fn post_process(&self, stmts: Vec<Stmt>) -> Res<Program> {
        let mut added_syslib = false;
        let mut added_floatlib = false;
        let mut stmts = self.add_stdlibs(stmts, &mut added_syslib, &mut added_floatlib);
        let nstmts = stmts.len();
        let srclines = stmts.iter().map(|s| s.props.srcline).collect_vec();
        // here we:
        // - determine the "abstain" type of each statement
        // - add "way to" info for the next srcline
        // - create a map of all labels to logical lines
        // - count polite statements
        // - set the correct "on the way to" line for error statements
        // - collect variables for renaming
        let mut npolite = 0;
        let mut stmt_types = Vec::new();
        let mut labels = BTreeMap::new();
        let mut comefroms: HashMap<usize, u16> = HashMap::new();
        let mut vars = Vars { counts: vec![0, 0, 0, 0], map: HashMap::new() };
        for (i, mut stmt) in stmts.iter_mut().enumerate() {
            stmt_types.push(stmt.stype());
            stmt.props.onthewayto =
                if i < nstmts - 1 { srclines[i + 1] } else { srclines[i] };
            if stmt.props.label > 0 {
                if labels.contains_key(&stmt.props.label) {
                    return Err(IE182.mk(None, stmt.props.onthewayto));
                }
                labels.insert(stmt.props.label, i as u16);
            }
            if stmt.props.polite {
                npolite += 1;
            }
            if let StmtBody::Error(e) = &mut stmt.body {
                e.set_line(stmt.props.onthewayto);
            }
            self.collect_vars(&mut vars, &mut stmt);
        }
        // check politeness
        if stmts.len() > 2 {
            if npolite == 0 || (stmts.len() - 1) / npolite >= 5 {
                return IE079.err();
            } else if stmts.len() / npolite < 3 {
                return IE099.err();
            }
        }
        // here we:
        // - create a map of all come-froms to logical lines
        // - apply new variable names
        // - make sure abstain labels exist
        // - make sure TRY AGAIN is last in the file
        let mut uses_complex_comefrom = false;
        for (i, mut stmt) in stmts.iter_mut().enumerate() {
            if let StmtBody::ComeFrom(spec) = &stmt.body {
                match spec {
                    ComeFrom::Label(n) => {
                        match labels.get(n) {
                            None => return Err(IE444.mk(None, stmt.props.onthewayto)),
                            Some(j) => {
                                if comefroms.contains_key(&(*j as usize)) {
                                    return Err(IE555.mk(None, stmt.props.onthewayto));
                                }
                                comefroms.insert(*j as usize, i as u16);
                            }
                        }
                    }
                    ComeFrom::Gerund(g) => {
                        for (j, stype) in stmt_types.iter().enumerate() {
                            if *g == *stype {
                                if comefroms.contains_key(&j) {
                                    return Err(IE555.mk(None, stmt.props.onthewayto));
                                }
                                comefroms.insert(j, i as u16);
                            }
                        }
                    }
                    ComeFrom::Expr(_) => {
                        uses_complex_comefrom = true;
                    }
                }
            }
            self.rename_vars(&vars, &mut stmt);
            if let StmtBody::Abstain(_, v) = &stmt.body {
                if let Abstain::Label(n) = v[0] {
                    if !labels.contains_key(&n) {
                        return Err(IE139.mk(None, stmt.props.onthewayto));
                    }
                }
            }
            if let StmtBody::TryAgain = stmt.body {
                // TRY AGAIN must be the last statement in the file
                if i != nstmts - 1 {
                    return Err(IE993.mk(None, stmt.props.onthewayto));
                }
            }
        }
        // here we:
        // - assign comefroms to statements
        for (i, mut stmt) in stmts.iter_mut().enumerate() {
            stmt.comefrom = comefroms.remove(&i);
        }
        // select a line for the compiler bug
        let mut rng = rand::thread_rng();
        let bugline = if self.allow_bug && rng.gen_range(0..10) == 0 {
            rng.gen_range(0..stmts.len())
        } else {
            stmts.len()  // can never be reached
        } as u16;
        // collect variable counts
        let var_info = (vec![VarInfo::new(); vars.counts[0]],
                        vec![VarInfo::new(); vars.counts[1]],
                        vec![VarInfo::new(); vars.counts[2]],
                        vec![VarInfo::new(); vars.counts[3]]);
        Ok(Program { stmts,
                     labels,
                     stmt_types,
                     var_info,
                     uses_complex_comefrom,
                     added_syslib,
                     added_floatlib,
                     bugline })
    }
}


#[derive(Debug)]
struct Vars {
    counts: Vec<usize>,
    map: HashMap<(u8, usize), usize>,
}
