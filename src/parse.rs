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
use std::io::{ Read, BufRead, BufReader, Cursor };
use std::u16;

use ast;
use err;
use lex::{ lex, LexerIter, TT };


enum Error {
    Hard(err::Error),
    Soft(usize),
}

type ParseRes<T> = Result<T, Error>;

pub struct Parser<'p> {
    lines: Vec<String>,
    tokens: LexerIter<Cursor<&'p [u8]>>,
}


impl<'p> Parser<'p> {
    pub fn new(code: &Vec<u8>) -> Parser {
        let cursor1 = Cursor::new(&code[..]);
        let lines = BufReader::new(cursor1).lines().map(|v| v.unwrap()).collect();
        let cursor2 = Cursor::new(&code[..]);
        Parser { lines: lines, tokens: lex(cursor2) }
    }

    pub fn parse(&mut self) -> Result<ast::Program, err::Error> {
        // a program is a series of statements until EOF
        let mut stmts = Vec::new();
        loop {
            if self.tokens.peek().is_none() {
                break;
            }
            stmts.push(try!(self.parse_stmt()));
        }
        // collect some necessary values and return the Program
        self.post_process(stmts)
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, err::Error> {
        let mut props = ast::StmtProps::default();
        // try to decode a statement
        match self.parse_stmt_maybe(&mut props) {
            // a hard error while parsing (rare)
            Err(Error::Hard(err)) => Err(err),
            // a "soft" error: thrown at runtime as E000
            Err(Error::Soft(srcline)) => {
                let body = ast::StmtBody::Error(
                    err::full(&err::IE000, Some(self.lines[srcline].clone()), srcline));
                // jump over tokens until the next statement beginning
                loop {
                    match self.tokens.peek() {
                        None |
                        Some(&TT::WAX) |
                        Some(&TT::DO) |
                        Some(&TT::PLEASEDO) => break,
                        _ => { self.tokens.next(); }
                    }
                }
                // return the botched statement
                Ok(ast::Stmt { body: body, props: props })
            },
            // a full statement!
            Ok(body) => Ok(ast::Stmt { body: body, props: props }),
        }
    }

    fn parse_stmt_maybe(&mut self, props: &mut ast::StmtProps) -> ParseRes<ast::StmtBody> {
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
            let schance = try!(self.req_number(100, &err::IE017));
            props.chance = schance as u8;
        }
        // parse statement meat
        // assignment?
        if let Some(var) = try!(self.parse_var_maybe(true)) {
            try!(self.req(TT::GETS));
            let expr = try!(self.parse_expr());
            return Ok(ast::StmtBody::Calc(var, expr));
        }
        // next jump?
        if let Some(lno) = try!(self.parse_label_maybe()) {
            try!(self.req(TT::NEXT));
            return Ok(ast::StmtBody::DoNext(lno));
        }
        // other statements headed by keyword
        if self.take(TT::COMEFROM) {
            let lno = try!(self.parse_label());
            Ok(ast::StmtBody::ComeFrom(lno))
        } else if self.take(TT::RESUME) {
            Ok(ast::StmtBody::Resume(try!(self.parse_expr())))
        } else if self.take(TT::FORGET) {
            Ok(ast::StmtBody::Forget(try!(self.parse_expr())))
        } else if self.take(TT::IGNORE) {
            Ok(ast::StmtBody::Ignore(try!(self.parse_varlist())))
        } else if self.take(TT::REMEMBER) {
            Ok(ast::StmtBody::Remember(try!(self.parse_varlist())))
        } else if self.take(TT::STASH) {
            Ok(ast::StmtBody::Stash(try!(self.parse_varlist())))
        } else if self.take(TT::RETRIEVE) {
            Ok(ast::StmtBody::Retrieve(try!(self.parse_varlist())))
        } else if self.take(TT::ABSTAIN) {
            Ok(ast::StmtBody::Abstain(try!(self.parse_abstain())))
        } else if self.take(TT::REINSTATE) {
            Ok(ast::StmtBody::Reinstate(try!(self.parse_abstain())))
        } else if self.take(TT::WRITEIN) {
            Ok(ast::StmtBody::WriteIn(try!(self.parse_var(true))))
        } else if self.take(TT::READOUT) {
            Ok(ast::StmtBody::ReadOut(try!(self.parse_expr())))
        } else if self.take(TT::GIVEUP) {
            return Ok(ast::StmtBody::GiveUp)
        } else {
            Err(self.invalid())
        }
    }

    fn parse_label_maybe(&mut self) -> ParseRes<Option<u16>> {
        if self.take(TT::WAX) {
            let lno = try!(self.req_number(u16::MAX, &err::IE197));
            try!(self.req(TT::WANE));
            Ok(Some(lno))
        } else {
            Ok(None)
        }
    }

    fn parse_label(&mut self) -> ParseRes<u16> {
        try!(self.parse_label_maybe()).ok_or_else(|| self.invalid())
    }

    fn parse_var_maybe(&mut self, subs_allowed: bool) -> ParseRes<Option<ast::Var>> {
        if self.take(TT::SPOT) {
            let val = try!(self.req_number(u16::MAX, &err::IE200));
            return Ok(Some(ast::Var::I16(val)));
        }
        if self.take(TT::TWOSPOT) {
            let val = try!(self.req_number(u16::MAX, &err::IE200));
            return Ok(Some(ast::Var::I32(val)));
        }
        if self.take(TT::TAIL) {
            let val = try!(self.req_number(u16::MAX, &err::IE200));
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(ast::Var::A16(val, subs)));
        }
        if self.take(TT::HYBRID) {
            let val = try!(self.req_number(u16::MAX, &err::IE200));
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(ast::Var::A32(val, subs)));
        }
        return Ok(None);
    }

    fn parse_var(&mut self, subs_allowed: bool) -> ParseRes<ast::Var> {
        try!(self.parse_var_maybe(subs_allowed)).ok_or_else(|| self.invalid())
    }

    fn parse_subs(&mut self) -> ParseRes<Vec<ast::Expr>> {
        let mut res = Vec::new();
        if self.take(TT::SUB) {
            while let Ok(expr) = self.parse_expr() {
                res.push(expr);
            }
        }
        Ok(res)
    }

    fn parse_varlist(&mut self) -> ParseRes<Vec<ast::Var>> {
        let mut res = Vec::new();
        res.push(try!(self.parse_var(false)));
        while self.take(TT::INTERSECTION) {
            res.push(try!(self.parse_var(false)));
        }
        Ok(res)
    }

    fn parse_abstain(&mut self) -> ParseRes<ast::Abstain> {
        if let Some(lno) = try!(self.parse_label_maybe()) {
            return Ok(ast::Abstain::Line(lno));
        }
        if self.take(TT::CALCULATING) {
            Ok(ast::Abstain::Calc)
        } else if self.take(TT::NEXTING) {
            Ok(ast::Abstain::Next)
        } else if self.take(TT::RESUMING) {
            Ok(ast::Abstain::Resume)
        } else if self.take(TT::FORGETTING) {
            Ok(ast::Abstain::Forget)
        } else if self.take(TT::IGNORING) {
            Ok(ast::Abstain::Ignore)
        } else if self.take(TT::REMEMBERING) {
            Ok(ast::Abstain::Remember)
        } else if self.take(TT::STASHING) {
            Ok(ast::Abstain::Stash)
        } else if self.take(TT::RETRIEVING) {
            Ok(ast::Abstain::Retrieve)
        } else if self.take(TT::ABSTAINING) {
            Ok(ast::Abstain::Abstain)
        } else if self.take(TT::REINSTATING) {
            Ok(ast::Abstain::Reinstate)
        } else if self.take(TT::COMINGFROM) {
            Ok(ast::Abstain::ComeFrom)
        } else if self.take(TT::READINGOUT) {
            Ok(ast::Abstain::ReadOut)
        } else if self.take(TT::WRITINGIN) {
            Ok(ast::Abstain::WriteIn)
        } else {
            Err(self.invalid())
        }
    }

    fn parse_expr(&mut self) -> ParseRes<ast::Expr> {
        let left = try!(self.parse_expr2());
        if self.take(TT::MONEY) {
            let right = try!(self.parse_expr2());
            return Ok(ast::Expr::Mingle(box left, box right));
        }
        if self.take(TT::SQUIGGLE) {
            let right = try!(self.parse_expr2());
            return Ok(ast::Expr::Select(box left, box right));
        }
        return Ok(left);
    }

    fn parse_expr2(&mut self) -> ParseRes<ast::Expr> {
        if self.take(TT::MESH) {
            let val = try!(self.req_number(u16::MAX, &err::IE017));
            return Ok(ast::Expr::Num(ast::Val::I16(val)))
        }
        if let Some(var) = try!(self.parse_var_maybe(true)) {
            return Ok(ast::Expr::Var(var));
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
            Ok(ast::Expr::And(box expr))
        } else if self.take(TT::BOOK) {
            let expr = try!(self.parse_expr());
            Ok(ast::Expr::Or(box expr))
        } else if self.take(TT::WHAT) {
            let expr = try!(self.parse_expr());
            Ok(ast::Expr::Xor(box expr))
        } else {
            Err(self.invalid())
        }
    }

    #[inline]
    fn take(&mut self, t: TT) -> bool {
        match self.tokens.peek() {
            Some(ref v) if **v == t => { },
            _ => return false,
        }
        self.tokens.next();
        true
    }

    fn req_number(&mut self, max: u16, err: &'static err::ErrDesc) -> ParseRes<u16> {
        match self.tokens.next() {
            Some(TT::NUMBER(x)) => {
                if x > max as u32 {
                    Err(Error::Hard(err::with_line(err, self.tokens.lineno())))
                } else {
                    Ok(x as u16)
                }
            },
            Some(t)  => {
                self.tokens.push(t);
                Err(self.invalid())
            }
            None     => Err(self.invalid()),
        }
    }

    fn req(&mut self, t: TT) -> ParseRes<()> {
        match self.tokens.next() {
            None                    => Err(self.invalid()),
            Some(ref x) if *x == t  => Ok(()),
            Some(t)                 => {
                self.tokens.push(t);
                Err(self.invalid())
            }
        }
    }

    #[inline]
    fn invalid(&mut self) -> Error {
        Error::Soft(self.tokens.lineno())
    }

    fn post_process(&self, stmts: Vec<ast::Stmt>) -> Result<ast::Program, err::Error> {
        // here we collect:
        // - the "abstain" type of each statement
        // - a map of all labels to logical lines
        // - a map of all come-froms to logical lines
        let mut types = Vec::new();
        let mut labels = HashMap::new();
        let mut comefroms = HashMap::new();
        for (i, stmt) in stmts.iter().enumerate() {
            types.push(stmt.stype());
            if stmt.props.label > 0 {
                labels.insert(stmt.props.label, i as u16);
            }
        }
        for (i, stmt) in stmts.iter().enumerate() {
            if let ast::StmtBody::ComeFrom(n) = stmt.body {
                if !labels.contains_key(&n) {
                    return Err(err::with_line(&err::IE444, stmt.props.srcline));
                }
                if comefroms.contains_key(&n) {
                    return Err(err::with_line(&err::IE555, stmt.props.srcline));
                }
                comefroms.insert(n, i as u16);
            }
        }
        Ok(ast::Program { stmts: stmts,
                          labels: labels,
                          comefroms: comefroms,
                          stmt_types: types })
    }
}
