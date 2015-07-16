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

use std::io::{ Read, BufRead, BufReader, Cursor };

use ast;
use err;
use lex::{ lex, LexerIter, Token, TT };


enum Error {
    IcErr(err::Error),
    ParseErr(usize),
}

type Res<T> = Result<T, Error>;

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
        let mut stmts = Vec::new();
        loop {
            if self.tokens.peek().is_none() { break; }
            stmts.push(try!(self.parse_stmt()));
        }
        Ok(ast::Program(stmts))
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, err::Error> {
        let mut props = ast::StmtProps::default();
        match self.parse_stmt_begin(&mut props) {
            Err(Error::IcErr(e)) => return Err(e),
            Err(Error::ParseErr(srcline)) => {
                let stmt = ast::Stmt(ast::StmtType::Error(
                    err::full(&err::IE000,
                              Some(self.lines[srcline].clone()),
                              self.tokens.peek().unwrap().1)), props);
                self.recover_splat();
                return Ok(stmt);
            },
            Ok(()) => { },
        }
        match self.parse_stmt_inner() {
            Err(Error::IcErr(e)) => Err(e),
            Err(Error::ParseErr(srcline)) => {
                let stmt = ast::Stmt(ast::StmtType::Error(
                    err::full(&err::IE000,
                              Some(self.lines[srcline].clone()),
                              self.tokens.peek().unwrap().1)), props);
                self.recover_splat();
                return Ok(stmt);
            },
            Ok(s) => Ok(ast::Stmt(s, props)),
        }
    }

    fn recover_splat(&mut self) {
        loop {
            match self.tokens.peek() {
                None |
                Some(&Token(TT::LABEL(_), _)) |
                Some(&Token(TT::DO(_), _)) => break,
                _ => { self.tokens.next(); }
            }
        }
    }

    fn parse_stmt_begin(&mut self, props: &mut ast::StmtProps) -> Res<()> {
        // parse logical line number
        if let Some(&Token(TT::LABEL(lno), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if lno > 65535 {
                return self.compiler_err(&err::IE197, "", tok);
            }
            props.logline = lno as u16;
        }
        // parse statement inititiator
        if let Some(Token(TT::DO(polite), lno)) = self.tokens.next() {
            props.srcline = lno;
            props.polite = polite;
        } else {
            return self.parse_err();
        }
        // parse initial disable
        if let Some(_) = self.take(TT::NOT) {
            props.disabled = true;
        }
        // parse percentage
        if let Some(tok) = self.take(TT::OHOHSEVEN) {
            let schance = try!(self.req_number());
            if schance > 100 {
                return self.compiler_err(&err::IE017, "", tok);
            }
            props.chance = schance as u8;
        }
        Ok(())
    }

    fn parse_stmt_inner(&mut self) -> Res<ast::StmtType> {
        // parse statement interior
        if let Some(var) = try!(self.parse_var(true)) {
            try!(self.req(TT::GETS));
            let expr = try!(self.parse_expr());
            return Ok(ast::StmtType::Calc(var, expr));
        }
        if let Some(&Token(TT::LABEL(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE197, "", tok);
            }
            try!(self.req(TT::NEXT));
            return Ok(ast::StmtType::DoNext(val as u16));
        }
        if let Some(_) = self.take(TT::COMEFROM) {
            if let Some(&Token(TT::LABEL(val), _)) = self.tokens.peek() {
                let tok = self.tokens.next().unwrap();
                if val > 65535 {
                    return self.compiler_err(&err::IE197, "", tok);
                }
                return Ok(ast::StmtType::ComeFrom(val as u16));
            } else {
                return self.parse_err();
            }
        }
        if let Some(_) = self.take(TT::RESUME) {
            return Ok(ast::StmtType::Resume(try!(self.parse_expr())));
        }
        if let Some(_) = self.take(TT::FORGET) {
            return Ok(ast::StmtType::Forget(try!(self.parse_expr())));
        }
        if let Some(_) = self.take(TT::STASH) {
            return Ok(ast::StmtType::Stash(try!(self.parse_stashlist())));
        }
        if let Some(_) = self.take(TT::RETRIEVE) {
            return Ok(ast::StmtType::Retrieve(try!(self.parse_stashlist())));
        }
        if let Some(_) = self.take(TT::ABSTAIN) {
            return Ok(ast::StmtType::Abstain(try!(self.parse_abstain())));
        }
        if let Some(_) = self.take(TT::REINSTATE) {
            return Ok(ast::StmtType::Reinstate(try!(self.parse_abstain())));
        }
        if let Some(_) = self.take(TT::WRITEIN) {
            if let Some(var) = try!(self.parse_var(true)) {
                return Ok(ast::StmtType::WriteIn(var));
            } else {
                return self.parse_err();
            }
        }
        if let Some(_) = self.take(TT::READOUT) {
            if let Some(var) = try!(self.parse_var(true)) {
                return Ok(ast::StmtType::ReadOut(var));
            } else {
                return self.parse_err();
            }
        }
        if let Some(_) = self.take(TT::GIVEUP) {
            return Ok(ast::StmtType::GiveUp);
        }
        self.parse_err()
    }

    fn parse_var(&mut self, subs_allowed: bool) -> Res<Option<ast::Var>> {
        if let Some(&Token(TT::SPOT(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE200, "", tok);
            }
            return Ok(Some(ast::Var::I16(val as u16)));
        }
        if let Some(&Token(TT::TWOSPOT(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE200, "", tok);
            }
            return Ok(Some(ast::Var::I32(val as u16)));
        }
        if let Some(&Token(TT::TAIL(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE200, "", tok);
            }
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(ast::Var::A16(val as u16, subs)));
        }
        if let Some(&Token(TT::HYBRID(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE200, "", tok);
            }
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(ast::Var::A32(val as u16, subs)));
        }
        return Ok(None);
    }

    fn parse_subs(&mut self) -> Res<Vec<ast::Expr>> {
        let mut res = Vec::new();
        while let Some(_) = self.take(TT::SUB) {
            res.push(try!(self.parse_expr()));
        }
        Ok(res)
    }

    fn parse_stashlist(&mut self) -> Res<Vec<ast::Var>> {
        let mut res = Vec::new();
        match try!(self.parse_var(false)) {
            None => return self.parse_err(),
            Some(v) => res.push(v),
        }
        while let Some(_) = self.take(TT::INTERSECTION) {
            match try!(self.parse_var(false)) {
                None => return self.parse_err(),
                Some(v) => res.push(v),
            }
        }
        Ok(res)
    }

    fn parse_abstain(&mut self) -> Res<ast::Abstain> {
        if let Some(&Token(TT::LABEL(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE197, "", tok);
            }
            return Ok(ast::Abstain::Line(val as u16));
        }
        if let Some(_) = self.take(TT::CALCULATING) {
            return Ok(ast::Abstain::Calc);
        }
        if let Some(_) = self.take(TT::NEXTING) {
            return Ok(ast::Abstain::Next);
        }
        if let Some(_) = self.take(TT::RESUMING) {
            return Ok(ast::Abstain::Resume);
        }
        if let Some(_) = self.take(TT::FORGETTING) {
            return Ok(ast::Abstain::Forget);
        }
        if let Some(_) = self.take(TT::REMEMBERING) {
            return Ok(ast::Abstain::Remember);
        }
        if let Some(_) = self.take(TT::STASHING) {
            return Ok(ast::Abstain::Stash);
        }
        if let Some(_) = self.take(TT::RETRIEVING) {
            return Ok(ast::Abstain::Retrieve);
        }
        if let Some(_) = self.take(TT::ABSTAINING) {
            return Ok(ast::Abstain::Abstain);
        }
        if let Some(_) = self.take(TT::REINSTATING) {
            return Ok(ast::Abstain::Reinstate);
        }
        if let Some(_) = self.take(TT::COMINGFROM) {
            return Ok(ast::Abstain::ComeFrom);
        }
        if let Some(_) = self.take(TT::READINGOUT) {
            return Ok(ast::Abstain::ReadOut);
        }
        if let Some(_) = self.take(TT::WRITINGIN) {
            return Ok(ast::Abstain::WriteIn);
        }
        self.parse_err()
    }

    fn parse_expr(&mut self) -> Res<ast::Expr> {
        let at = try!(self.parse_expr2());
        if let Some(_) = self.take(TT::MONEY) {
            let at2 = try!(self.parse_expr2());
            return Ok(ast::Expr::Mingle(Box::new(at), Box::new(at2)));
        }
        if let Some(_) = self.take(TT::SQUIGGLE) {
            let at2 = try!(self.parse_expr2());
            return Ok(ast::Expr::Select(Box::new(at), Box::new(at2)));
        }
        return Ok(at);
    }

    fn parse_expr2(&mut self) -> Res<ast::Expr> {
        if let Some(&Token(TT::MESH(val), _)) = self.tokens.peek() {
            let tok = self.tokens.next().unwrap();
            if val > 65535 {
                return self.compiler_err(&err::IE017, "", tok);
            }
            return Ok(ast::Expr::Num(val as u16))
        }
        if let Some(var) = try!(self.parse_var(true)) {
            return Ok(ast::Expr::Var(var));
        }
        if let Some(_) = self.take(TT::RABBITEARS) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::RABBITEARS));
            return Ok(expr);
        }
        if let Some(_) = self.take(TT::SPARK) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::SPARK));
            return Ok(expr);
        }
        if let Some(_) = self.take(TT::AMPERSAND) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::And(Box::new(expr)));
        }
        if let Some(_) = self.take(TT::BOOK) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::Or(Box::new(expr)));
        }
        if let Some(_) = self.take(TT::WHAT) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::Xor(Box::new(expr)));
        }
        self.parse_err()
    }

    fn take(&mut self, t: TT) -> Option<Token> {
        match self.tokens.peek() {
            Some(&Token(ref v, _)) if *v == t => { },
            _ => return None,
        }
        Some(self.tokens.next().unwrap())
    }

    fn req_number(&mut self) -> Res<u32> {
        match self.tokens.next() {
            None                          => self.parse_err(),
            Some(Token(TT::NUMBER(x), _)) => Ok(x),
            Some(t)                       => {
                self.tokens.push(t);
                self.parse_err()
            }
        }
    }

    fn req(&mut self, t: TT) -> Res<()> {
        match self.tokens.next() {
            None                     => self.parse_err(),
            Some(ref x) if x.0 == t  => Ok(()),
            Some(t)                  => {
                self.tokens.push(t);
                self.parse_err()
            }
        }
    }

    fn parse_err<T>(&mut self) -> Res<T> {
        Err(Error::ParseErr(self.tokens.peek().unwrap().1))
    }

    fn compiler_err<T>(&self, error: &'static err::ErrDesc, msg: &str, tok: Token) -> Res<T> {
        if msg == "" {
            Err(Error::IcErr(err::full(error, Some(msg.into()), tok.1)))
        } else {
            Err(Error::IcErr(err::with_line(error, tok.1)))
        }
    }
}
