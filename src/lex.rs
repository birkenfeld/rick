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

use pest::Parser;
use pest::iterators::Pairs;

pub type SrcLine = usize;

/// A lexer for INTERCAL generated with Pest.
///
/// The raw Pest lexer is wrapped by a buffer iterator that adds a few
/// special methods, such as the pretty standard "peek" and "push back" features.

#[derive(Parser)]
#[grammar = "lex.pest"]
struct PestLexer;

pub struct SrcToken {
    pub line: SrcLine,
    pub rule: Rule,
    pub value: u32  // for NUMBER tokens
}

pub struct Lexer<'a> {
    inner:     Pairs<'a, Rule>,
    startline: SrcLine,
    stash:     Vec<SrcToken>,
    lastline:  SrcLine,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = SrcToken;

    fn next(&mut self) -> Option<Self::Item> {
        let ret = self.inner_next();
        if let Some(tok) = ret {
            self.lastline = tok.line;
            return Some(tok);
        }
        None

    }
}

impl<'a> Lexer<'a> {
    fn inner_next(&mut self) -> Option<SrcToken> {
        self.stash.pop().or_else(|| {
            self.inner.next().map(|pair| {
                let rule = pair.as_rule();
                let text = pair.as_str();
                let line = pair.into_span().end_pos().line_col().0 - 1 + self.startline;
                // convert into SrcToken
                if rule == Rule::NUMBER {
                    SrcToken { line, rule, value: text.trim().parse().unwrap_or(u32::max_value()) }
                } else if rule == Rule::WOW {
                    // handle ! = '. combination
                    self.stash.push(SrcToken { line: line, rule: Rule::SPOT, value: 0 });
                    SrcToken { line, rule: Rule::SPARK, value: 0 }
                } else {
                    SrcToken { line, rule, value: 0 }
                }
            })
        })
    }

    pub fn peek(&mut self) -> Option<Rule> {
        if !self.stash.is_empty() {
            return self.stash.last().map(|v| v.rule);
        }
        match self.inner_next() {
            None => None,
            Some(tok) => {
                self.stash.push(tok);
                self.stash.last().map(|v| v.rule)
            }
        }
    }

    pub fn push(&mut self, t: SrcToken) {
        self.stash.push(t);
    }

    pub fn lineno(&self) -> SrcLine {
        self.lastline
    }
}

pub fn lex(s: &str, startline: usize) -> Lexer {
    // always succeeds since we have an UNKNOWN token
    let inner = PestLexer::parse(Rule::tokens, s).unwrap();
    Lexer { inner, startline, stash: vec![], lastline: startline }
}
