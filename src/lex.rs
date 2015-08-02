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


#[derive(PartialEq, Eq, Clone, Debug)]
pub struct Token(pub TT, pub usize);

#[derive(PartialEq, Eq, Clone, Debug)]
pub enum TT {
    // special tokens
    UNKNOWN(String),
    NUMBER(u32),
    LABEL(u32),
    DO(bool),
    NOT,

    // sigils
    SPOT(u32),
    TWOSPOT(u32),
    TAIL(u32),
    HYBRID(u32),
    WOW(u32),
    MESH(u32),

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

    // misc. symbols
    GETS,
    SUB,
    BY,
    OHOHSEVEN,
    INTERSECTION,

    // precedence
    SPARK,
    RABBITEARS,

    // bit operators
    MONEY,
    SQUIGGLE,
    AMPERSAND,
    BOOK,
    WHAT,
}


type Lx<'a, R> = &'a mut IcLexer<R>;

rustlex! IcLexer {
    property line: usize = 0;

    let NUM = ['0'-'9']+;
    let WS = [' ' '\t']+;
    let NL = '\n';
    let ANY = .;
    let LBL = '(' ['0'-'9']+ ')';
    let SPOT = '.' ['0'-'9']+;
    let SPOT2 = ':' ['0'-'9']+;
    let TAIL = ',' ['0'-'9']+;
    let TAIL2 = ';' ['0'-'9']+;
    let WOW = '!' ['0'-'9']+;
    let MESH = '#' ['0'-'9']+;

    ANY   => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::UNKNOWN(s)) }
    WS    => |_: Lx<R>| -> Option<Token> { None }
    NL    => |l: Lx<R>| -> Option<Token> { l.line += 1; None }
    NUM   => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::NUMBER(s.parse().unwrap())) }
    LBL   => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::LABEL(s[1..s.len()-1].parse().unwrap())) }

    "PLEASE"       => |l: Lx<R>| l.tok(TT::DO(true))
    "PLEASE DO"    => |l: Lx<R>| l.tok(TT::DO(true))
    "DO"           => |l: Lx<R>| l.tok(TT::DO(false))
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

    SPOT  => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::SPOT(s[1..].parse().unwrap())) }
    SPOT2 => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::TWOSPOT(s[1..].parse().unwrap())) }
    TAIL  => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::TAIL(s[1..].parse().unwrap())) }
    TAIL2 => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::HYBRID(s[1..].parse().unwrap())) }
    WOW   => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::WOW(s[1..].parse().unwrap())) }
    MESH  => |l: Lx<R>| { let s = l.yystr(); l.tok(TT::MESH(s[1..].parse().unwrap())) }

    "<-"  => |l: Lx<R>| l.tok(TT::GETS)
    "SUB" => |l: Lx<R>| l.tok(TT::SUB)
    "BY"  => |l: Lx<R>| l.tok(TT::BY)
    '%'   => |l: Lx<R>| l.tok(TT::OHOHSEVEN)
    '+'   => |l: Lx<R>| l.tok(TT::INTERSECTION)

    '"'   => |l: Lx<R>| l.tok(TT::RABBITEARS)
    '\''  => |l: Lx<R>| l.tok(TT::SPARK)

    '$'   => |l: Lx<R>| l.tok(TT::MONEY)
    '~'   => |l: Lx<R>| l.tok(TT::SQUIGGLE)
    '&'   => |l: Lx<R>| l.tok(TT::AMPERSAND)
    'V'   => |l: Lx<R>| l.tok(TT::BOOK)
    '?'   => |l: Lx<R>| l.tok(TT::WHAT)
}

impl<R: Read> IcLexer<R> {
    #[inline]
    fn tok(&self, t: TT) -> Option<Token> {
        Some(Token(t, self.line))
    }
}


pub struct LexerIter<R: Read> {
    inner: IcLexer<R>,
    unkbuf: Option<String>,
    unklno: usize,
    stash: Vec<Token>,
}

impl<R: Read> Iterator for LexerIter<R> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        // if we have some tokens stashed, just emit them
        while !self.stash.is_empty() {
            return self.stash.pop();
        }
        while let Some(mut tok) = self.inner.next() {
            // handle ! = '. combination right now
            if let Token(TT::WOW(val), _) = tok {
                self.stash.push(Token(TT::SPOT(val), tok.1));
                tok = Token(TT::SPARK, tok.1);
            }
            // unknowns: push them onto the buffer
            if let Token(TT::UNKNOWN(s), l) = tok {
                match self.unkbuf {
                    Some(ref mut buf) => buf.push_str(&s),
                    None => { self.unkbuf = Some(s); self.unklno = l; },
                }
                continue;
            }
            // not an unknown: output the collected unknowns
            if self.unkbuf.is_some() {
                self.stash.push(tok);
                return self.unkbuf.take().map(|v| Token(TT::UNKNOWN(v), self.unklno));
            }
            // else just return the current token
            return Some(tok);
        }
        // no token anymore, check if unknown buffer has something
        if self.unkbuf.is_some() {
            return self.unkbuf.take().map(|v| Token(TT::UNKNOWN(v), self.unklno));
        }
        // finally, nothing left
        return None;
    }
}

impl<R: Read> LexerIter<R> {

    pub fn peek(&mut self) -> Option<&Token> {
        if !self.stash.is_empty() {
            return self.stash.last();
        }
        match self.next() {
            None => None,
            Some(tok) => {
                self.stash.push(tok);
                self.stash.last()
            }
        }
    }

    pub fn push(&mut self, t: Token) {
        self.stash.push(t);
    }
}


pub fn lex<R: Read>(reader: R) -> LexerIter<R> {
    let lexer = IcLexer::new(reader);
    LexerIter { inner: lexer, unkbuf: None, unklno: 0, stash: vec![] }
}
