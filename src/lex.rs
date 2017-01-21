// -------------------------------------------------------------------------------------------------
// Rick, a Rust intercal compiler.  Save your souls!
//
// Copyright (c) 2015-2017 Georg Brandl
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

#![allow(non_snake_case)]

/// A lexer for INTERCAL generated with RustLex.
///
/// The raw RustLex lexer is wrapped by a buffer iterator that adds a few
/// special methods, such as the pretty standard "peek" and "push back" features.

use pest::prelude::*;


pub type SrcLine = usize;


impl_rdp! {
    grammar! {
        whitespace   =  { ([" "] | ["\n"] | ["\t"])+ }

        token        = _{ NUMBER | syntax | gerund | verb | designator | operator |
                          whitespace | UNKNOWN }

        NUMBER       =  { ['0'..'9']+ }
        UNKNOWN      =  { any }

        syntax       = _{ WAX | WANE | PLEASEDO | DO | NOT | GETS | SUB | BY |
                          OHOHSEVEN | INTERSECTION | WOW | MESH }
        WAX          =  { ["("] }
        WANE         =  { [")"] }
        PLEASEDO     =  { ["PLEASE"] ~ ["DO"]? }
        DO           =  { ["DO"] }
        NOT          =  { ["NOT"] | ["N'T"] }
        GETS         =  { ["<-"] }
        SUB          =  { ["SUB"] }
        BY           =  { ["BY"] }
        OHOHSEVEN    =  { ["%"] }
        INTERSECTION =  { ["+"] }
        WOW          =  { ["!"] }
        MESH         =  { ["#"] }

        verb         = _{ NEXT | RESUME | FORGET | IGNORE | REMEMBER | STASH |
                          RETRIEVE | ABSTAIN | FROM | REINSTATE | COMEFROM |
                          READOUT | WRITEIN | TRYAGAIN | GIVEUP }
        NEXT         =  { ["NEXT"] }
        RESUME       =  { ["RESUME"] }
        FORGET       =  { ["FORGET"] }
        IGNORE       =  { ["IGNORE"] }
        REMEMBER     =  { ["REMEMBER"] }
        STASH        =  { ["STASH"] }
        RETRIEVE     =  { ["RETRIEVE"] }
        ABSTAIN      =  { ["ABSTAIN"] }
        FROM         =  { ["FROM"] }
        REINSTATE    =  { ["REINSTATE"] }
        COMEFROM     =  { ["COME"] ~ ["FROM"] }
        READOUT      =  { ["READ"] ~ ["OUT"] }
        WRITEIN      =  { ["WRITE"] ~ ["IN"] }
        TRYAGAIN     =  { ["TRY"] ~ ["AGAIN"] }
        GIVEUP       =  { ["GIVE"] ~ ["UP"] }

        gerund       = _{ CALCULATING | NEXTING | RESUMING | FORGETTING |
                          IGNORING | REMEMBERING | STASHING | RETRIEVING |
                          ABSTAINING | REINSTATING | COMINGFROM | READINGOUT |
                          WRITINGIN | TRYINGAGAIN }
        CALCULATING  =  { ["CALCULATING"] }
        NEXTING      =  { ["NEXTING"] }
        RESUMING     =  { ["RESUMING"] }
        FORGETTING   =  { ["FORGETTING"] }
        IGNORING     =  { ["IGNORING"] }
        REMEMBERING  =  { ["REMEMBERING"] }
        STASHING     =  { ["STASHING"] }
        RETRIEVING   =  { ["RETRIEVING"] }
        ABSTAINING   =  { ["ABSTAINING"] }
        REINSTATING  =  { ["REINSTATING"] }
        COMINGFROM   =  { ["COMING"] ~ ["FROM"] }
        READINGOUT   =  { ["READING"] ~ ["OUT"] }
        WRITINGIN    =  { ["WRITING"] ~ ["IN"] }
        TRYINGAGAIN  =  { ["TRYING"] ~ ["AGAIN"] }

        designator   = _{ SPOT | TWOSPOT | TAIL | HYBRID }
        SPOT         =  { ["."] }
        TWOSPOT      =  { [":"] }
        TAIL         =  { [","] }
        HYBRID       =  { [";"] }

        operator     = _{ RABBITEARS | SPARK | MONEY | SQUIGGLE |
                          AMPERSAND | BOOK | WHAT }
        RABBITEARS   =  { ["\""] }
        SPARK        =  { ["'"] }
        MONEY        =  { ["$"] | ["¢"] | ["¤"] | ["£"] | ["€"] }
        SQUIGGLE     =  { ["~"] }
        AMPERSAND    =  { ["&"] }
        BOOK         =  { ["V"] }
        WHAT         =  { ["?"] | ["∀"] }
    }
}

// impl<R: Read> RawLexer<R> {
//     #[inline]
//     fn tok(&self, t: TT) -> Option<Token> {
//         Some(Token(t, self.line))
//     }

//     #[inline]
//     fn tok_with_nl(&mut self, t: TT) -> Option<Token> {
//         let ret = Token(t, self.line);
//         let newlines = self.yystr().chars().filter(|c| *c == '\n').count();
//         self.line += newlines;
//         Some(ret)
//     }
// }


pub struct SrcToken(Rule, u32);

impl SrcToken {
    pub fn rule(&self) -> Rule { self.0 }
    pub fn value(&self) -> u32 { self.1 }
}

pub struct Lexer<'a> {
    rdp:      Rdp<StringInput<'a>>,
    rdpline:  SrcLine,
    stash:    Vec<(SrcToken, SrcLine)>,
    lastline: SrcLine,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = SrcToken;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner_next();
        if let Some(tok) = ret {
            self.lastline = tok.1;
            return Some(tok.0);
        }
        None

    }
}

impl<'a> Lexer<'a> {
    fn inner_next(&mut self) -> Option<(SrcToken, SrcLine)> {
        // if we have some tokens stashed, just emit them
        if !self.stash.is_empty() {
            return self.stash.pop();
        }
        // else, request a new token from the lexer
        while self.rdp.token() {
            let line = self.rdpline;
            while let Some(tok) = self.rdp.queue_mut().pop() {
                if tok.rule == Rule::whitespace {
                    self.rdpline += self.rdp.input().slice(tok.start, tok.end)
                                                    .chars().filter(|&ch| ch == '\n').count();
                    continue;
                }
                // println!("l{} - {:?}", line, tok);
                let srctoken = if tok.rule == Rule::NUMBER {
                    SrcToken(Rule::NUMBER, self.rdp.input().slice(tok.start, tok.end)
                             .parse().unwrap())
                } else if tok.rule == Rule::WOW {
                    // handle ! = '. combination
                    self.stash.push((SrcToken(Rule::SPOT, 0), line));
                    SrcToken(Rule::SPARK, 0)
                } else {
                    SrcToken(tok.rule, 0)
                };
                return Some((srctoken, line));
            }
        }
        None
    }

    pub fn peek(&mut self) -> Option<&Rule> {
        if !self.stash.is_empty() {
            return self.stash.last().map(|v| &(v.0).0);
        }
        match self.inner_next() {
            None => None,
            Some(tok) => {
                self.stash.push(tok);
                self.stash.last().map(|v| &(v.0).0)
            }
        }
    }

    pub fn push(&mut self, t: SrcToken) {
        self.stash.push((t, self.lastline));
    }

    pub fn lineno(&self) -> SrcLine {
        self.lastline
    }
}

pub fn lex<'a>(s: &'a str, startline: usize) -> Lexer<'a> {
    let input = StringInput::new(s);
    Lexer { rdp: Rdp::new(input), rdpline: startline,
            stash: vec![], lastline: startline }
}
