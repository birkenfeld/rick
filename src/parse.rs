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

/// Parses INTERCAL (yes, it's possible!) and generates an Abstract Sadism Tree.
///
/// Hand-written, since I didn't find any parser generators that looked nice enough.
/// The INTERCAL syntax is actually not that complicated (except where it is), but
/// the various extensions make it a bit iffy.
///
/// There are quite a few steps to do after parsing, which are done in the method
/// called `post_process`.  It makes a list of statements into a "real" program.

use std::collections::{ BTreeMap, HashMap };
use std::io::{ Read, BufRead, BufReader, Cursor };
use std::u16;

use rand::{ self, Rng };

use ast::{ self, Program, Stmt, StmtBody, StmtProps, Expr, Abstain, ComeFrom, Var, VType, VarInfo };
use err::{ Res, RtError, ErrDesc, IE000, IE017, IE079, IE099, IE139, IE182, IE197, IE200,
           IE444, IE555, IE993 };
use lex::{ lex, Lexer, SrcLine, TT };
use syslib;


enum DecodeError {
    Hard(RtError),
    Soft(SrcLine),
}

type ParseRes<T> = Result<T, DecodeError>;

pub struct Parser<'p> {
    lines:  Vec<String>,
    tokens: Lexer<Cursor<&'p [u8]>>,
    stash:  Vec<TT>,  // used for backtracking
    startline: usize,
    allow_bug: bool,
}


impl<'p> Parser<'p> {
    pub fn new(code: &Vec<u8>, startline: usize, allow_bug: bool) -> Parser {
        let cursor1 = Cursor::new(&code[..]);
        // we have to keep a list of all physical source lines to generate
        // E000 error messages, so duplicate the input stream
        let lines = Parser::get_lines(BufReader::new(cursor1));
        let cursor2 = Cursor::new(&code[..]);
        Parser { lines: lines,
                 tokens: lex(cursor2, startline),
                 stash: Vec::new(),
                 startline: startline,
                 allow_bug: allow_bug }
    }

    fn get_lines<T: Read>(mut reader: BufReader<T>) -> Vec<String> {
        let mut buf = Vec::new();
        let mut res = Vec::new();
        while let Ok(n) = reader.read_until('\n' as u8, &mut buf) {
            if n == 0 {
                break;
            }
            let end = if buf[n - 1] == '\n' as u8 { n - 1 } else { n };
            res.push(String::from_utf8_lossy(&buf[..end]).into_owned());
            buf.clear();
        }
        res
    }

    /// Parse the whole file as a program.
    pub fn get_program(&mut self) -> Res<Program> {
        // parse all statements
        let stmts = try!(self.parse());
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
            stmts.push(try!(self.parse_stmt()));
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
                    IE000.new(Some(self.lines[srcline - self.startline].clone()), 0));
                // jump over tokens until the next statement beginning
                loop {
                    match self.tokens.peek() {
                        None |
                        Some(&TT::DO) |
                        Some(&TT::PLEASEDO) => break,
                        Some(&TT::WAX) => {
                            let wax = self.tokens.next().expect("THERE WAX A TOKEN I SWEAR");
                            if let Some(&TT::NUMBER(_)) = self.tokens.peek() {
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
                Ok(Stmt { body: body, props: props, comefrom: None, can_abstain: true })
            }
            // a full statement!
            Ok(body) => {
                let can_abstain = body != StmtBody::GiveUp;
                Ok(Stmt { body: body, props: props, comefrom: None,
                          can_abstain: can_abstain })
            }
        }
    }

    /// Try to parse a full statement.
    fn parse_stmt_maybe(&mut self, props: &mut StmtProps) -> ParseRes<StmtBody> {
        // parse logical line number label
        if let Some(label) = try!(self.parse_label_maybe()) {
            props.label = label;
        }
        // parse statement inititiator
        if self.take(TT::DO) {
            props.srcline = self.tokens.lineno();
        } else if self.take(TT::PLEASEDO) {
            props.srcline = self.tokens.lineno();
            props.polite = true;
        } else {
            return Err(self.invalid());
        }
        // parse initial disable
        if self.take(TT::NOT) {
            props.disabled = true;
        }
        // parse percentage
        if self.take(TT::OHOHSEVEN) {
            let schance = try!(self.req_number(100, &IE017));
            props.chance = schance as u8;
        }
        // parse statement meat
        // assignment?
        if let Some(var) = try!(self.parse_var_maybe(true)) {
            try!(self.req(TT::GETS));
            if var.is_dim() {
                let exprs = try!(self.parse_by_exprs());
                return Ok(StmtBody::Dim(var, exprs));
            } else {
                let expr = try!(self.parse_expr());
                return Ok(StmtBody::Calc(var, expr));
            }
        }
        // next jump?
        if let Some(lbl) = try!(self.parse_label_maybe()) {
            try!(self.req(TT::NEXT));
            return Ok(StmtBody::DoNext(lbl));
        }
        // other statements headed by keyword
        if self.take(TT::COMEFROM) {
            if let Some(lbl) = try!(self.parse_label_maybe()) {
                Ok(StmtBody::ComeFrom(ComeFrom::Label(lbl)))
            } else if let Ok(gerund) = self.parse_gerund() {
                Ok(StmtBody::ComeFrom(ComeFrom::Gerund(gerund)))
            } else {
                Ok(StmtBody::ComeFrom(ComeFrom::Expr(try!(self.parse_expr()))))
            }
        } else if self.take(TT::RESUME) {
            Ok(StmtBody::Resume(try!(self.parse_expr())))
        } else if self.take(TT::FORGET) {
            Ok(StmtBody::Forget(try!(self.parse_expr())))
        } else if self.take(TT::IGNORE) {
            Ok(StmtBody::Ignore(try!(self.parse_varlist(false))))
        } else if self.take(TT::REMEMBER) {
            Ok(StmtBody::Remember(try!(self.parse_varlist(false))))
        } else if self.take(TT::STASH) {
            Ok(StmtBody::Stash(try!(self.parse_varlist(false))))
        } else if self.take(TT::RETRIEVE) {
            Ok(StmtBody::Retrieve(try!(self.parse_varlist(false))))
        } else if self.take(TT::ABSTAIN) {
            Ok(try!(self.parse_abstain()))
        } else if self.take(TT::REINSTATE) {
            Ok(StmtBody::Reinstate(try!(self.parse_abstain_items())))
        } else if self.take(TT::WRITEIN) {
            Ok(StmtBody::WriteIn(try!(self.parse_varlist(true))))
        } else if self.take(TT::READOUT) {
            Ok(StmtBody::ReadOut(try!(self.parse_readlist())))
        } else if self.take(TT::TRYAGAIN) {
            Ok(StmtBody::TryAgain)
        } else if self.take(TT::GIVEUP) {
            Ok(StmtBody::GiveUp)
        } else {
            Err(self.invalid())
        }
    }

    /// Maybe parse a line label (N).
    fn parse_label_maybe(&mut self) -> ParseRes<Option<ast::Label>> {
        if self.take(TT::WAX) {
            let lbl = try!(self.req_number(u16::MAX, &IE197));
            try!(self.req(TT::WANE));
            Ok(Some(lbl))
        } else {
            Ok(None)
        }
    }

    /// Maybe parse a variable reference [.:,;]N {SUB X}.
    fn parse_var_maybe(&mut self, subs_allowed: bool) -> ParseRes<Option<Var>> {
        if self.take(TT::SPOT) {
            let val = try!(self.req_number(u16::MAX, &IE200));
            return Ok(Some(Var::I16(val as usize)));
        }
        if self.take(TT::TWOSPOT) {
            let val = try!(self.req_number(u16::MAX, &IE200));
            return Ok(Some(Var::I32(val as usize)));
        }
        if self.take(TT::TAIL) {
            let val = try!(self.req_number(u16::MAX, &IE200));
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(Var::A16(val as usize, subs)));
        }
        if self.take(TT::HYBRID) {
            let val = try!(self.req_number(u16::MAX, &IE200));
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(Var::A32(val as usize, subs)));
        }
        Ok(None)
    }

    /// Require a variable reference.
    fn parse_var(&mut self, subs_allowed: bool) -> ParseRes<Var> {
        try!(self.parse_var_maybe(subs_allowed)).ok_or_else(|| self.invalid())
    }

    /// Parse subscripts for a variable reference.
    fn parse_subs(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = Vec::new();
        if self.take(TT::SUB) {
            // we need one expr at least
            res.push(try!(self.parse_expr()));
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
        let mut res = Vec::new();
        res.push(try!(self.parse_var(subs_allowed)));
        while self.take(TT::INTERSECTION) {
            res.push(try!(self.parse_var(subs_allowed)));
        }
        Ok(res)
    }

    /// Parse a list of variables (with subscripts) or consts separated by +.
    fn parse_readlist(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = Vec::new();
        if self.take(TT::MESH) {
            let val = try!(self.req_number(u16::MAX, &IE017));
            res.push(Expr::Num(VType::I16, val as u32));
        } else {
            res.push(Expr::Var(try!(self.parse_var(true))));
        }
        while self.take(TT::INTERSECTION) {
            if self.take(TT::MESH) {
                let val = try!(self.req_number(u16::MAX, &IE017));
                res.push(Expr::Num(VType::I16, val as u32));
            } else {
                res.push(Expr::Var(try!(self.parse_var(true))));
            }
        }
        Ok(res)
    }

    /// Maybe parse a variable reference with maybe inline unary op [.:,;] OP N {SUB X}.
    fn parse_item_with_unop(&mut self) -> ParseRes<Option<Expr>> {
        fn parse_constr(self_: &mut Parser) -> Box<Fn(Expr) -> Expr> {
            if self_.take(TT::AMPERSAND) {
                box |e| Expr::And(VType::I16, box e)
            } else if self_.take(TT::BOOK) {
                box |e| Expr::Or(VType::I16, box e)
            } else if self_.take(TT::WHAT) {
                box |e| Expr::Xor(VType::I16, box e)
            } else {
                box |e| e
            }
        }
        if self.take(TT::MESH) {
            let constr = parse_constr(self);
            let val = try!(self.req_number(u16::MAX, &IE017));
            return Ok(Some(constr(Expr::Num(VType::I16, val as u32))));
        }
        if self.take(TT::SPOT) {
            let constr = parse_constr(self);
            let val = try!(self.req_number(u16::MAX, &IE200));
            return Ok(Some(constr(Expr::Var(Var::I16(val as usize)))));
        }
        if self.take(TT::TWOSPOT) {
            let constr = parse_constr(self);
            let val = try!(self.req_number(u16::MAX, &IE200));
            return Ok(Some(constr(Expr::Var(Var::I32(val as usize)))));
        }
        if self.take(TT::TAIL) {
            let constr = parse_constr(self);
            let val = try!(self.req_number(u16::MAX, &IE200));
            let subs = try!(self.parse_subs());
            return Ok(Some(constr(Expr::Var(Var::A16(val as usize, subs)))));
        }
        if self.take(TT::HYBRID) {
            let constr = parse_constr(self);
            let val = try!(self.req_number(u16::MAX, &IE200));
            let subs = try!(self.parse_subs());
            return Ok(Some(constr(Expr::Var(Var::A32(val as usize, subs)))));
        }
        Ok(None)
    }

    /// Parse an ABSTAIN statement.
    fn parse_abstain(&mut self) -> ParseRes<StmtBody> {
        let mut expr = None;
        if !self.take(TT::FROM) {
            expr = Some(try!(self.parse_expr()));
            try!(self.req(TT::FROM));
        }
        Ok(StmtBody::Abstain(expr, try!(self.parse_abstain_items())))
    }

    /// Parse items following ABSTAIN FROM or REINSTATE.
    fn parse_abstain_items(&mut self) -> ParseRes<Vec<Abstain>> {
        // first form: a single line label
        if let Some(lbl) = try!(self.parse_label_maybe()) {
            return Ok(vec![Abstain::Label(lbl)]);
        }
        // second form: one or more gerunds
        let mut res = Vec::new();
        res.push(try!(self.parse_gerund()));
        while self.take(TT::INTERSECTION) {
            res.push(try!(self.parse_gerund()));
        }
        Ok(res)
    }

    /// Parse a single gerund after ABSTAIN or REINSTATE.
    fn parse_gerund(&mut self) -> ParseRes<Abstain> {
        if self.take(TT::CALCULATING) {
            Ok(Abstain::Calc)
        } else if self.take(TT::NEXTING) {
            Ok(Abstain::Next)
        } else if self.take(TT::RESUMING) {
            Ok(Abstain::Resume)
        } else if self.take(TT::FORGETTING) {
            Ok(Abstain::Forget)
        } else if self.take(TT::IGNORING) {
            Ok(Abstain::Ignore)
        } else if self.take(TT::REMEMBERING) {
            Ok(Abstain::Remember)
        } else if self.take(TT::STASHING) {
            Ok(Abstain::Stash)
        } else if self.take(TT::RETRIEVING) {
            Ok(Abstain::Retrieve)
        } else if self.take(TT::ABSTAINING) {
            Ok(Abstain::Abstain)
        } else if self.take(TT::REINSTATING) {
            Ok(Abstain::Reinstate)
        } else if self.take(TT::COMINGFROM) {
            Ok(Abstain::ComeFrom)
        } else if self.take(TT::READINGOUT) {
            Ok(Abstain::ReadOut)
        } else if self.take(TT::WRITINGIN) {
            Ok(Abstain::WriteIn)
        } else if self.take(TT::TRYINGAGAIN) {
            Ok(Abstain::TryAgain)
        } else {
            Err(self.invalid())
        }
    }

    /// Parse a list of exprs separated by BY.
    fn parse_by_exprs(&mut self) -> ParseRes<Vec<Expr>> {
        let mut res = Vec::new();
        res.push(try!(self.parse_expr()));
        while self.take(TT::BY) {
            res.push(try!(self.parse_expr()));
        }
        Ok(res)
    }

    /// Parse a single expression.
    fn parse_expr(&mut self) -> ParseRes<Expr> {
        let left = try!(self.parse_expr2());
        if self.take(TT::MONEY) {
            let right = try!(self.parse_expr2());
            return Ok(Expr::Mingle(box left, box right));
        }
        if self.take(TT::SQUIGGLE) {
            let right = try!(self.parse_expr2());
            return Ok(Expr::Select(right.get_vtype(), box left, box right));
        }
        Ok(left)
    }

    fn parse_expr2(&mut self) -> ParseRes<Expr> {
        if let Some(expr) = try!(self.parse_item_with_unop()) {
            return Ok(expr);
        }
        if self.take(TT::RABBITEARS) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::RABBITEARS));
            Ok(expr)
        } else if self.take(TT::SPARK) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::SPARK));
            Ok(expr)
        } else if self.take(TT::AMPERSAND) {
            let expr = try!(self.parse_expr());
            Ok(Expr::And(expr.get_vtype(), box expr))
        } else if self.take(TT::BOOK) {
            let expr = try!(self.parse_expr());
            Ok(Expr::Or(expr.get_vtype(), box expr))
        } else if self.take(TT::WHAT) {
            let expr = try!(self.parse_expr());
            Ok(Expr::Xor(expr.get_vtype(), box expr))
        } else {
            Err(self.invalid())
        }
    }

    /// If the next token is `t`, consume it and return true.
    #[inline]
    fn take(&mut self, t: TT) -> bool {
        match self.tokens.peek() {
            Some(ref v) if **v == t => { }
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
            Some(TT::NUMBER(x)) => {
                if x > max as u32 {
                    Err(DecodeError::Hard(err.new(None, self.tokens.lineno())))
                } else {
                    self.stash.push(TT::NUMBER(x));
                    Ok(x as u16)
                }
            }
            Some(t)  => {
                self.tokens.push(t);
                Err(self.invalid())
            }
            None     => Err(self.invalid()),
        }
    }

    /// Require a token `t` next.
    fn req(&mut self, t: TT) -> ParseRes<()> {
        match self.tokens.next() {
            None                    => Err(self.invalid()),
            Some(ref x) if *x == t  => {
                self.stash.push(t);
                Ok(())
            }
            Some(t)                 => {
                self.tokens.push(t);
                Err(self.invalid())
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
                if n >= 1000 && n < 1999 && need_syslib > -1 {
                    need_syslib = 1;
                }
                if n >= 5000 && n < 5999 && need_floatlib > -1 {
                    need_floatlib = 1;
                }
            }
        }
        let mut last_lineno = self.tokens.lineno();
        if need_syslib == 1 {
            let code = syslib::SYSLIB_CODE.to_vec();
            let mut p = Parser::new(&code, last_lineno, false);
            let mut syslib_stmts = p.parse().expect("E-42 SYSLIB BROKEN");
            stmts.append(&mut syslib_stmts);
            *added_syslib = true;
            last_lineno = p.tokens.lineno();
        }
        if need_floatlib == 1 {
            let code = syslib::FLOATLIB_CODE.to_vec();
            let mut p = Parser::new(&code, last_lineno, false);
            let mut floatlib_stmts = p.parse().expect("E2.0000000001 FLOATLIB BROKEN");
            stmts.append(&mut floatlib_stmts);
            *added_floatlib = true;
        }
        stmts
    }

    /// Walk all references to variables, and call a visitor function for each.
    fn walk_vars<F>(&self, stmt: &mut Stmt, mut visitor: F)
        where F: FnMut(&mut Var) -> ()
    {
        let visitor = &mut visitor;

        fn walk_var<F>(var: &mut Var, visitor: &mut F)
            where F: FnMut(&mut Var) -> ()
        {
            visitor(var);
            match *var {
                Var::A16(_, ref mut es) |
                Var::A32(_, ref mut es) => {
                    for e in es {
                        walk_expr(e, visitor);
                    }
                }
                Var::I16(_) |
                Var::I32(_) => { }
            }
        }

        fn walk_expr<F>(expr: &mut Expr, visitor: &mut F)
            where F: FnMut(&mut Var) -> ()
        {
            match *expr {
                Expr::Var(ref mut v) => walk_var(v, visitor),
                Expr::And(_, ref mut e) |
                Expr::Or(_, ref mut e) |
                Expr::Xor(_, ref mut e) |
                Expr::RsNot(ref mut e) => walk_expr(e, visitor),
                Expr::Mingle(ref mut e, ref mut e2) |
                Expr::Select(_, ref mut e, ref mut e2) |
                Expr::RsAnd(ref mut e, ref mut e2) |
                Expr::RsOr(ref mut e, ref mut e2) |
                Expr::RsXor(ref mut e, ref mut e2) |
                Expr::RsRshift(ref mut e, ref mut e2) |
                Expr::RsLshift(ref mut e, ref mut e2) |
                // Expr::RsEqual(ref mut e, ref mut e2) |
                Expr::RsNotEqual(ref mut e, ref mut e2) |
                Expr::RsPlus(ref mut e, ref mut e2) |
                Expr::RsMinus(ref mut e, ref mut e2) => {
                    walk_expr(e, visitor);
                    walk_expr(e2, visitor);
                }
                Expr::Num(_, _) => { }
            }
        }

        match stmt.body {
            StmtBody::Calc(ref mut v, ref mut e) => {
                walk_var(v, visitor);
                walk_expr(e, visitor);
            }
            StmtBody::Dim(ref mut v, ref mut es) => {
                walk_var(v, visitor);
                for e in es {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::Resume(ref mut e) |
            StmtBody::Forget(ref mut e) => {
                walk_expr(e, visitor);
            }
            StmtBody::Abstain(ref mut maybe_e, _) => {
                for e in maybe_e.iter_mut() {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::ComeFrom(ref mut spec) => {
                if let ComeFrom::Expr(ref mut e) = *spec {
                    walk_expr(e, visitor);
                }
            }
            StmtBody::Ignore(ref mut vs) |
            StmtBody::Remember(ref mut vs) |
            StmtBody::Stash(ref mut vs) |
            StmtBody::Retrieve(ref mut vs) |
            StmtBody::WriteIn(ref mut vs) => {
                for v in vs {
                    walk_var(v, visitor);
                }
            }
            StmtBody::ReadOut(ref mut es) => {
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
            if !vars.map.contains_key(&key) {
                let idx = key.0 as usize;
                vars.map.insert(key, vars.counts[idx]);
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
        let srclines = stmts.iter().map(|s| s.props.srcline).collect::<Vec<_>>();
        // here we:
        // - determine the "abstain" type of each statement
        // - add "way to" info for the next srcline
        // - create a map of all labels to logical lines
        // - count polite statements
        // - set the correct "on the way to" line for error statements
        // - collect variables for renaming
        let mut npolite = 0;
        let mut types = Vec::new();
        let mut labels = BTreeMap::new();
        let mut comefroms: HashMap<usize, u16> = HashMap::new();
        let mut vars = Vars { counts: vec![0, 0, 0, 0], map: HashMap::new() };
        for (i, mut stmt) in stmts.iter_mut().enumerate() {
            types.push(stmt.stype());
            stmt.props.onthewayto =
                if i < nstmts - 1 { srclines[i + 1] } else { srclines[i] };
            if stmt.props.label > 0 {
                if labels.contains_key(&stmt.props.label) {
                    return Err(IE182.new(None, stmt.props.onthewayto));
                }
                labels.insert(stmt.props.label, i as u16);
            }
            if stmt.props.polite {
                npolite += 1;
            }
            if let StmtBody::Error(ref mut e) = stmt.body {
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
            if let StmtBody::ComeFrom(ref spec) = stmt.body {
                match *spec {
                    ComeFrom::Label(n) => {
                        match labels.get(&n) {
                            None => return Err(IE444.new(None, stmt.props.onthewayto)),
                            Some(j) => {
                                if comefroms.contains_key(&(*j as usize)) {
                                    return Err(IE555.new(None, stmt.props.onthewayto));
                                }
                                comefroms.insert(*j as usize, i as u16);
                            }
                        }
                    }
                    ComeFrom::Gerund(ref g) => {
                        for (j, stype) in types.iter().enumerate() {
                            if *g == *stype {
                                if comefroms.contains_key(&j) {
                                    return Err(IE555.new(None, stmt.props.onthewayto));
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
            if let StmtBody::Abstain(_, ref v) = stmt.body {
                if let Abstain::Label(n) = v[0] {
                    if !labels.contains_key(&n) {
                        return Err(IE139.new(None, stmt.props.onthewayto));
                    }
                }
            }
            if let StmtBody::TryAgain = stmt.body {
                // TRY AGAIN must be the last statement in the file
                if i != nstmts - 1 {
                    return Err(IE993.new(None, stmt.props.onthewayto));
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
        let bugline = if self.allow_bug && rng.gen_range(0, 10) == 0 {
            rng.gen_range(0, stmts.len())
        } else {
            stmts.len()  // can never be reached
        } as u16;
        // collect variable counts
        let var_info = (vec![VarInfo::new(); vars.counts[0]],
                        vec![VarInfo::new(); vars.counts[1]],
                        vec![VarInfo::new(); vars.counts[2]],
                        vec![VarInfo::new(); vars.counts[3]]);
        Ok(Program { stmts: stmts,
                     labels: labels,
                     stmt_types: types,
                     var_info: var_info,
                     uses_complex_comefrom: uses_complex_comefrom,
                     added_syslib: added_syslib,
                     added_floatlib: added_floatlib,
                     bugline: bugline })
    }
}


#[derive(Debug)]
struct Vars {
    counts: Vec<usize>,
    map: HashMap<(u8, usize), usize>,
}
