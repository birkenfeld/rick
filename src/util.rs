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

use err;


/// Which roman digits from the digit_tbl to put together for each
/// decimal digit.
/// These are reversed because the whole digit string is reversed
/// in the end.
const ROMAN_TRANS_TBL: [(usize, [usize; 4]); 10] = [
    // (# of digits, which digits)
    (0, [0, 0, 0, 0]),
    (1, [0, 0, 0, 0]),
    (2, [0, 0, 0, 0]),
    (3, [0, 0, 0, 0]),
    (2, [2, 1, 0, 0]),    /* or use (4, [0, 0, 0, 0]) */
    (1, [2, 0, 0, 0]),
    (2, [1, 2, 0, 0]),
    (3, [1, 1, 2, 0]),
    (4, [1, 1, 1, 2]),
    (2, [3, 1, 0, 0])];

/// Which roman digits to use for each 10^n place.
const ROMAN_DIGIT_TBL: [[(char, char); 4]; 10] = [
    // (first line - overbars, second line - characters)
    [(' ', 'I'), (' ', 'I'), (' ', 'V'), (' ', 'X')],
    [(' ', 'X'), (' ', 'X'), (' ', 'L'), (' ', 'C')],
    [(' ', 'C'), (' ', 'C'), (' ', 'D'), (' ', 'M')],
    [(' ', 'M'), ('_', 'I'), ('_', 'V'), ('_', 'X')],
    [('_', 'X'), ('_', 'X'), ('_', 'L'), ('_', 'C')],
    [('_', 'C'), ('_', 'C'), ('_', 'D'), ('_', 'M')],
    [('_', 'M'), (' ', 'i'), (' ', 'v'), (' ', 'x')],
    [(' ', 'x'), (' ', 'x'), (' ', 'l'), (' ', 'c')],
    [(' ', 'c'), (' ', 'c'), (' ', 'd'), (' ', 'm')],
    [(' ', 'm'), ('_', 'i'), ('_', 'v'), ('_', 'x')]];


pub fn to_roman(mut val: u32) -> String {
    let mut l1 = Vec::new();
    let mut l2 = Vec::new();
    let mut place = 0;
    while val > 0 {
        let digit = (val % 10) as usize;
        for j in 0..ROMAN_TRANS_TBL[digit].0 {
            let idx = ROMAN_TRANS_TBL[digit].1[j];
            l1.push(ROMAN_DIGIT_TBL[place][idx].0);
            l2.push(ROMAN_DIGIT_TBL[place][idx].1);
        }
        place += 1;
        val /= 10;
    }
    l1.reverse();
    l2.reverse();
    format!("{}\n{}\n",
            l1.into_iter().collect::<String>(),
            l2.into_iter().collect::<String>())
}


const ENGLISH_DIGITS: [(&'static str, u8); 12] = [
    ("ZERO",  0),
    ("OH",    0),
    ("ONE",   1),
    ("TWO",   2),
    ("THREE", 3),
    ("FOUR",  4),
    ("FIVE",  5),
    ("SIX",   6),
    ("SEVEN", 7),
    ("EIGHT", 8),
    ("NINE",  9),
    ("NINER", 9)];

pub fn from_english(v: &str) -> Result<u32, err::Error> {
    let mut digits = Vec::new();
    for word in v.split_whitespace() {
        let mut found = false;
        for &(w, val) in &ENGLISH_DIGITS {
            if w == word {
                digits.push(val);
                found = true;
                break;
            }
        }
        if !found {
            return Err(err::with_str(&err::IE579, word));
        }
    }
    let mut res = 0;
    for (i, digit) in digits.iter().enumerate() {
        res += (*digit as u32) * (10 as u32).pow(digits.len() as u32 - 1 - i as u32);
    }
    Ok(res)
}


//pub fn mingle()
