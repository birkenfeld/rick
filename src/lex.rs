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

use std::io::Read;
use std::u32;


pub type SrcLine = usize;

#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Token(pub TT, pub SrcLine);

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TT {
    // any unknown non-whitespace character
    UNKNOWN,

    // an integer literal
    NUMBER(u32),

    // statement initiators
    WAX,
    WANE,
    DO,
    PLEASEDO,
    NOT,

    // sigils
    SPOT,
    TWOSPOT,
    TAIL,
    HYBRID,
    WOW,
    MESH,

    // grouping operators
    SPARK,
    RABBITEARS,

    // number operators
    MONEY,
    SQUIGGLE,
    AMPERSAND,
    BOOK,
    WHAT,

    // misc. symbols
    GETS,
    SUB,
    BY,
    OHOHSEVEN,
    INTERSECTION,

    // word stmt types
    NEXT,
    RESUME,
    FORGET,
    IGNORE,
    REMEMBER,
    STASH,
    RETRIEVE,
    ABSTAIN,
    REINSTATE,
    COMEFROM,
    READOUT,
    WRITEIN,
    GIVEUP,

    // gerunds for abstain/reinstate
    CALCULATING,
    NEXTING,
    RESUMING,
    IGNORING,
    FORGETTING,
    REMEMBERING,
    STASHING,
    RETRIEVING,
    ABSTAINING,
    REINSTATING,
    COMINGFROM,
    READINGOUT,
    WRITINGIN,
}


type Lx<'a, R> = &'a mut RawLexer<R>;

rustlex! RawLexer {
    property line: SrcLine = 1;

    let ANY = .;
    let NUM = ['0'-'9']+;
    let WS  = [' ' '\t']+;
    let NL  = '\n';

    ANY            => |l: Lx<R>| l.tok(TT::UNKNOWN)
    NUM            => |l: Lx<R>| { let s = l.yystr();
                                   l.tok(s.parse().map(TT::NUMBER)
                                         .unwrap_or(TT::NUMBER(u32::MAX))) }
    WS             => |_: Lx<R>| -> Option<Token> { None }
    NL             => |l: Lx<R>| -> Option<Token> { l.line += 1; None }

    '('            => |l: Lx<R>| l.tok(TT::WAX)
    ')'            => |l: Lx<R>| l.tok(TT::WANE)
    "PLEASE"       => |l: Lx<R>| l.tok(TT::PLEASEDO)
    "PLEASE DO"    => |l: Lx<R>| l.tok(TT::PLEASEDO)
    "DO"           => |l: Lx<R>| l.tok(TT::DO)
    "NOT"          => |l: Lx<R>| l.tok(TT::NOT)
    "N'T"          => |l: Lx<R>| l.tok(TT::NOT)

    "NEXT"         => |l: Lx<R>| l.tok(TT::NEXT)
    "RESUME"       => |l: Lx<R>| l.tok(TT::RESUME)
    "FORGET"       => |l: Lx<R>| l.tok(TT::FORGET)
    "IGNORE"       => |l: Lx<R>| l.tok(TT::IGNORE)
    "REMEMBER"     => |l: Lx<R>| l.tok(TT::REMEMBER)
    "STASH"        => |l: Lx<R>| l.tok(TT::STASH)
    "RETRIEVE"     => |l: Lx<R>| l.tok(TT::RETRIEVE)
    "ABSTAIN FROM" => |l: Lx<R>| l.tok(TT::ABSTAIN)
    "REINSTATE"    => |l: Lx<R>| l.tok(TT::REINSTATE)
    "COME FROM"    => |l: Lx<R>| l.tok(TT::COMEFROM)
    "READ OUT"     => |l: Lx<R>| l.tok(TT::READOUT)
    "WRITE IN"     => |l: Lx<R>| l.tok(TT::WRITEIN)
    "GIVE UP"      => |l: Lx<R>| l.tok(TT::GIVEUP)

    "CALCULATING"  => |l: Lx<R>| l.tok(TT::CALCULATING)
    "NEXTING"      => |l: Lx<R>| l.tok(TT::NEXTING)
    "RESUMING"     => |l: Lx<R>| l.tok(TT::RESUMING)
    "FORGETTING"   => |l: Lx<R>| l.tok(TT::FORGETTING)
    "IGNORING"     => |l: Lx<R>| l.tok(TT::IGNORING)
    "REMEMBERING"  => |l: Lx<R>| l.tok(TT::REMEMBERING)
    "STASHING"     => |l: Lx<R>| l.tok(TT::STASHING)
    "RETRIEVING"   => |l: Lx<R>| l.tok(TT::RETRIEVING)
    "ABSTAINING"   => |l: Lx<R>| l.tok(TT::ABSTAINING)
    "REINSTATING"  => |l: Lx<R>| l.tok(TT::REINSTATING)
    "COMING FROM"  => |l: Lx<R>| l.tok(TT::COMINGFROM)
    "READING OUT"  => |l: Lx<R>| l.tok(TT::READINGOUT)
    "WRITING IN"   => |l: Lx<R>| l.tok(TT::WRITINGIN)

    '.'            => |l: Lx<R>| l.tok(TT::SPOT)
    ':'            => |l: Lx<R>| l.tok(TT::TWOSPOT)
    ','            => |l: Lx<R>| l.tok(TT::TAIL)
    ';'            => |l: Lx<R>| l.tok(TT::HYBRID)
    '!'            => |l: Lx<R>| l.tok(TT::WOW)
    '#'            => |l: Lx<R>| l.tok(TT::MESH)

    "<-"           => |l: Lx<R>| l.tok(TT::GETS)
    "SUB"          => |l: Lx<R>| l.tok(TT::SUB)
    "BY"           => |l: Lx<R>| l.tok(TT::BY)
    '%'            => |l: Lx<R>| l.tok(TT::OHOHSEVEN)
    '+'            => |l: Lx<R>| l.tok(TT::INTERSECTION)

    '"'            => |l: Lx<R>| l.tok(TT::RABBITEARS)
    '\''           => |l: Lx<R>| l.tok(TT::SPARK)

    '$'            => |l: Lx<R>| l.tok(TT::MONEY)
    '~'            => |l: Lx<R>| l.tok(TT::SQUIGGLE)
    '&'            => |l: Lx<R>| l.tok(TT::AMPERSAND)
    'V'            => |l: Lx<R>| l.tok(TT::BOOK)
    '?'            => |l: Lx<R>| l.tok(TT::WHAT)
}

impl<R: Read> RawLexer<R> {
    #[inline]
    fn tok(&self, t: TT) -> Option<Token> {
        Some(Token(t, self.line))
    }
}


pub struct Lexer<R: Read> {
    inner: RawLexer<R>,
    stash: Vec<Token>,
    line:  SrcLine,
}

impl<R: Read> Iterator for Lexer<R> {
    type Item = TT;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner_next();
        if let Some(tok) = ret {
            self.line = tok.1;
            return Some(tok.0);
        }
        None
    }
}

impl<R: Read> Lexer<R> {
    fn inner_next(&mut self) -> Option<Token> {
        // if we have some tokens stashed, just emit them
        if !self.stash.is_empty() {
            return self.stash.pop();
        }
        // else, request a new token from the lexer
        if let Some(mut tok) = self.inner.next() {
            // handle ! = '. combination right now
            if let Token(TT::WOW, lno) = tok {
                self.stash.push(Token(TT::SPOT, lno));
                tok = Token(TT::SPARK, lno);
            }
            Some(tok)
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<&TT> {
        if !self.stash.is_empty() {
            return self.stash.last().map(|v| &v.0);
        }
        match self.inner_next() {
            None => None,
            Some(tok) => {
                self.stash.push(tok);
                self.stash.last().map(|v| &v.0)
            }
        }
    }

    pub fn push(&mut self, t: TT) {
        self.stash.push(Token(t, self.line));
    }

    pub fn lineno(&self) -> SrcLine {
        self.line
    }
}

pub fn lex<R: Read>(reader: R) -> Lexer<R> {
    let raw = RawLexer::new(reader);
    Lexer { inner: raw, stash: vec![], line: 1 }
}
