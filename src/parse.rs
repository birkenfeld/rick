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
        self.process(stmts)
    }

    fn parse_stmt(&mut self) -> Result<ast::Stmt, err::Error> {
        let mut props = ast::StmtProps::default();
        match self.parse_stmt_begin(&mut props) {
            Err(Error::IcErr(e)) => return Err(e),
            Err(Error::ParseErr(srcline)) => {
                // XXX
                let stype = ast::StmtType::Error(err::full(&err::IE000,
                                                           Some(self.lines[srcline].clone()),
                                                           self.tokens.lineno()));
                let stmt = ast::Stmt { st: stype, props: props };
                self.recover_splat();
                return Ok(stmt);
            },
            Ok(()) => { },
        }
        match self.parse_stmt_inner() {
            Err(Error::IcErr(e)) => Err(e),
            Err(Error::ParseErr(srcline)) => {
                let stype = ast::StmtType::Error(err::full(&err::IE000,
                                                           Some(self.lines[srcline].clone()),
                                                           self.tokens.lineno()));
                let stmt = ast::Stmt { st: stype, props: props };
                self.recover_splat();
                return Ok(stmt);
            },
            Ok(s) => Ok(ast::Stmt { st: s, props: props }),
        }
    }

    fn recover_splat(&mut self) {
        loop {
            match self.tokens.peek() {
                None |
                Some(&TT::WAX) |
                Some(&TT::DO) |
                Some(&TT::PLEASEDO) => break,
                _ => { self.tokens.next(); }
            }
        }
    }

    fn parse_stmt_begin(&mut self, props: &mut ast::StmtProps) -> Res<()> {
        // parse logical line number
        if self.take(TT::WAX) {
            let lno = try!(self.req_number());
            if lno > (u16::MAX as u32) {
                return self.parser_err(&err::IE197, "");
            }
            try!(self.req(TT::WANE));
            props.label = lno as u16;
        }
        // parse statement inititiator
        if self.take(TT::DO) {
            props.srcline = self.tokens.lineno();
        } else if self.take(TT::PLEASEDO) {
            props.srcline = self.tokens.lineno();
            props.polite = true;
        } else {
            return self.decode_err();
        }
        // parse initial disable
        if self.take(TT::NOT) {
            props.disabled = true;
        }
        // parse percentage
        if self.take(TT::OHOHSEVEN) {
            let schance = try!(self.req_number());
            if schance > 100 {
                return self.parser_err(&err::IE017, "");
            }
            props.chance = schance as u8;
        }
        Ok(())
    }

    fn parse_stmt_inner(&mut self) -> Res<ast::StmtType> {
        // parse statement interior
        if let Some(var) = try!(self.parse_var_maybe(true)) {
            try!(self.req(TT::GETS));
            let expr = try!(self.parse_expr());
            return Ok(ast::StmtType::Calc(var, expr));
        }
        if self.take(TT::WAX) {
            let lno = try!(self.req_number());
            if lno > (u16::MAX as u32) {
                return self.parser_err(&err::IE197, "");
            }
            try!(self.req(TT::WANE));
            try!(self.req(TT::NEXT));
            return Ok(ast::StmtType::DoNext(lno as u16));
        }
        if self.take(TT::COMEFROM) {
            try!(self.req(TT::WAX));
            let lno = try!(self.req_number());
            if lno > (u16::MAX as u32) {
                return self.parser_err(&err::IE197, "");
            }
            try!(self.req(TT::WANE));
            return Ok(ast::StmtType::ComeFrom(lno as u16));
        }
        if self.take(TT::RESUME) {
            return Ok(ast::StmtType::Resume(try!(self.parse_expr())));
        }
        if self.take(TT::FORGET) {
            return Ok(ast::StmtType::Forget(try!(self.parse_expr())));
        }
        if self.take(TT::IGNORE) {
            return Ok(ast::StmtType::Ignore(try!(self.parse_varlist())));
        }
        if self.take(TT::REMEMBER) {
            return Ok(ast::StmtType::Remember(try!(self.parse_varlist())));
        }
        if self.take(TT::STASH) {
            return Ok(ast::StmtType::Stash(try!(self.parse_varlist())));
        }
        if self.take(TT::RETRIEVE) {
            return Ok(ast::StmtType::Retrieve(try!(self.parse_varlist())));
        }
        if self.take(TT::ABSTAIN) {
            return Ok(ast::StmtType::Abstain(try!(self.parse_abstain())));
        }
        if self.take(TT::REINSTATE) {
            return Ok(ast::StmtType::Reinstate(try!(self.parse_abstain())));
        }
        if self.take(TT::WRITEIN) {
            return Ok(ast::StmtType::WriteIn(try!(self.parse_var(true))));
        }
        if self.take(TT::READOUT) {
            return Ok(ast::StmtType::ReadOut(try!(self.parse_expr())));
        }
        if self.take(TT::GIVEUP) {
            return Ok(ast::StmtType::GiveUp);
        }
        self.decode_err()
    }

    fn parse_var_maybe(&mut self, subs_allowed: bool) -> Res<Option<ast::Var>> {
        if self.take(TT::SPOT) {
            let val = try!(self.req_number());
            if val > (u16::MAX as u32) {
                return self.parser_err(&err::IE200, "");
            }
            return Ok(Some(ast::Var::I16(val as u16)));
        }
        if self.take(TT::TWOSPOT) {
            let val = try!(self.req_number());
            if val > (u16::MAX as u32) {
                return self.parser_err(&err::IE200, "");
            }
            return Ok(Some(ast::Var::I32(val as u16)));
        }
        if self.take(TT::TAIL) {
            let val = try!(self.req_number());
            if val > (u16::MAX as u32) {
                return self.parser_err(&err::IE200, "");
            }
            let subs = if subs_allowed {
                try!(self.parse_subs())
            } else {
                vec![]
            };
            return Ok(Some(ast::Var::A16(val as u16, subs)));
        }
        if self.take(TT::HYBRID) {
            let val = try!(self.req_number());
            if val > (u16::MAX as u32) {
                return self.parser_err(&err::IE200, "");
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

    fn parse_var(&mut self, subs_allowed: bool) -> Res<ast::Var> {
        if let Some(v) = try!(self.parse_var_maybe(subs_allowed)) {
            Ok(v)
        } else {
            self.decode_err()
        }
    }

    fn parse_subs(&mut self) -> Res<Vec<ast::Expr>> {
        let mut res = Vec::new();
        if self.take(TT::SUB) {
            while let Ok(expr) = self.parse_expr() {
                res.push(expr);
            }
        }
        Ok(res)
    }

    fn parse_varlist(&mut self) -> Res<Vec<ast::Var>> {
        let mut res = Vec::new();
        res.push(try!(self.parse_var(false)));
        while self.take(TT::INTERSECTION) {
            res.push(try!(self.parse_var(false)));
        }
        Ok(res)
    }

    fn parse_abstain(&mut self) -> Res<ast::Abstain> {
        if self.take(TT::WAX) {
            let lno = try!(self.req_number());
            if lno > (u16::MAX as u32) {
                return self.parser_err(&err::IE197, "");
            }
            try!(self.req(TT::WANE));
            return Ok(ast::Abstain::Line(lno as u16));
        }
        if self.take(TT::CALCULATING) {
            return Ok(ast::Abstain::Calc);
        }
        if self.take(TT::NEXTING) {
            return Ok(ast::Abstain::Next);
        }
        if self.take(TT::RESUMING) {
            return Ok(ast::Abstain::Resume);
        }
        if self.take(TT::FORGETTING) {
            return Ok(ast::Abstain::Forget);
        }
        if self.take(TT::IGNORING) {
            return Ok(ast::Abstain::Ignore);
        }
        if self.take(TT::REMEMBERING) {
            return Ok(ast::Abstain::Remember);
        }
        if self.take(TT::STASHING) {
            return Ok(ast::Abstain::Stash);
        }
        if self.take(TT::RETRIEVING) {
            return Ok(ast::Abstain::Retrieve);
        }
        if self.take(TT::ABSTAINING) {
            return Ok(ast::Abstain::Abstain);
        }
        if self.take(TT::REINSTATING) {
            return Ok(ast::Abstain::Reinstate);
        }
        if self.take(TT::COMINGFROM) {
            return Ok(ast::Abstain::ComeFrom);
        }
        if self.take(TT::READINGOUT) {
            return Ok(ast::Abstain::ReadOut);
        }
        if self.take(TT::WRITINGIN) {
            return Ok(ast::Abstain::WriteIn);
        }
        self.decode_err()
    }

    fn parse_expr(&mut self) -> Res<ast::Expr> {
        let at = try!(self.parse_expr2());
        if self.take(TT::MONEY) {
            let at2 = try!(self.parse_expr2());
            return Ok(ast::Expr::Mingle(box at, box at2));
        }
        if self.take(TT::SQUIGGLE) {
            let at2 = try!(self.parse_expr2());
            return Ok(ast::Expr::Select(box at, box at2));
        }
        return Ok(at);
    }

    fn parse_expr2(&mut self) -> Res<ast::Expr> {
        if self.take(TT::MESH) {
            let val = try!(self.req_number());
            if val > (u16::MAX as u32) {
                return self.parser_err(&err::IE017, "");
            }
            return Ok(ast::Expr::Num(ast::Val::I16(val as u16)))
        }
        if let Some(var) = try!(self.parse_var_maybe(true)) {
            return Ok(ast::Expr::Var(var));
        }
        if self.take(TT::RABBITEARS) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::RABBITEARS));
            return Ok(expr);
        }
        if self.take(TT::SPARK) {
            let expr = try!(self.parse_expr());
            try!(self.req(TT::SPARK));
            return Ok(expr);
        }
        if self.take(TT::AMPERSAND) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::And(box expr));
        }
        if self.take(TT::BOOK) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::Or(box expr));
        }
        if self.take(TT::WHAT) {
            let expr = try!(self.parse_expr());
            return Ok(ast::Expr::Xor(box expr));
        }
        self.decode_err()
    }

    fn take(&mut self, t: TT) -> bool {
        match self.tokens.peek() {
            Some(ref v) if **v == t => { },
            _ => return false,
        }
        self.tokens.next();
        true
    }

    fn req_number(&mut self) -> Res<u32> {
        match self.tokens.next() {
            None                => self.decode_err(),
            Some(TT::NUMBER(x)) => Ok(x),
            Some(t)             => {
                self.tokens.push(t);
                self.decode_err()
            }
        }
    }

    fn req(&mut self, t: TT) -> Res<()> {
        match self.tokens.next() {
            None                    => self.decode_err(),
            Some(ref x) if *x == t  => Ok(()),
            Some(t)                 => {
                self.tokens.push(t);
                self.decode_err()
            }
        }
    }

    fn decode_err<T>(&mut self) -> Res<T> {
        Err(Error::ParseErr(self.tokens.lineno()))
    }

    fn parser_err<T>(&self, error: &'static err::ErrDesc, msg: &str) -> Res<T> {
        if msg == "" {
            Err(Error::IcErr(err::full(error, Some(msg.into()),
                                       self.tokens.lineno())))
        } else {
            Err(Error::IcErr(err::with_line(error, self.tokens.lineno())))
        }
    }

    fn process(&self, stmts: Vec<ast::Stmt>) -> Result<ast::Program, err::Error> {
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
            if let ast::StmtType::ComeFrom(n) = stmt.st {
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
