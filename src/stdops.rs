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

/// Runtime support for INTERCAL compiler and interpreter.
///
/// This file provides code that is useful in the interpreter and in the compiled
/// program.
///
/// The basic things implemented here are:
///
/// * Bind, the struct holding INTERCAL variables and their STASHes
/// * Array, the struct holding INTERCAL arrays
/// * RNG for execution chances
/// * jump handling for RESUME and FORGET
/// * Roman numeral and English spelled-out number conversion
/// * Basic read/write of numbers and bytes
/// * all the INTERCAL operators (mingle, select, unary and, unary or, unary xor)

use std::fmt::{Debug, Display, Error, Formatter};
use std::fs::File;
use std::io::{Read, Write, stdin};
use std::{u16, u32};

use crate::err::{Res, IE240, IE241, IE252, IE436, IE533, IE562, IE579, IE621, IE632};

#[derive(Clone, Debug)]
pub struct Array<T> {
    pub dims: Vec<usize>,
    pub elems: Vec<T>,
}

impl<T: Clone + Default> Array<T> {
    pub fn new(dims: Vec<usize>) -> Array<T> {
        let total = dims.iter().product();
        let value = Default::default();
        Array { dims, elems: vec![value; total] }
    }

    pub fn empty() -> Array<T> {
        Array { dims: vec![], elems: vec![] }
    }
}

#[derive(Clone, Debug)]
pub struct Bind<T> {
    pub val: T,
    pub stack: Vec<T>,
    pub rw: bool,
}

impl<T: Clone> Bind<T> {
    pub fn new(t: T) -> Bind<T> {
        Bind { val: t, stack: Vec::new(), rw: true }
    }

    pub fn assign(&mut self, v: T) {
        if self.rw {
            self.val = v;
        }
    }

    #[allow(dead_code)]  // only used in compiled code
    pub fn assign_unchecked(&mut self, v: T) {
        self.val = v;
    }

    pub fn stash(&mut self) {
        self.stack.push(self.val.clone());
    }

    pub fn retrieve(&mut self, line: usize) -> Res<()> {
        match self.stack.pop() {
            None => IE436.err_with(None, line),
            Some(v) => {
                if self.rw {
                    self.val = v;
                }
                Ok(())
            }
        }
    }
}

impl<T: LikeU16 + Default> Bind<Array<T>> {
    pub fn set_md(&mut self, subs: &[usize], val: T, line: usize) -> Res<()> {
        let ix = self.get_index(subs, line)?;
        if self.rw {
            self.val.elems[ix] = val;
        }
        Ok(())
    }

    #[allow(dead_code)]  // only used in compiled code
    pub fn set(&mut self, sub: usize, val: T, line: usize) -> Res<()> {
        if self.val.dims.len() != 1 || sub > self.val.dims[0] {
            return IE241.err_with(None, line);
        }
        if self.rw {
            self.val.elems[sub - 1] = val;
        }
        Ok(())
    }

    #[allow(dead_code)]  // only used in compiled code
    pub fn set_md_unchecked(&mut self, subs: &[usize], val: T, line: usize) -> Res<()> {
        let ix = self.get_index(subs, line)?;
        self.val.elems[ix] = val;
        Ok(())
    }

    #[allow(dead_code)]  // only used in compiled code
    pub fn set_unchecked(&mut self, sub: usize, val: T, line: usize) -> Res<()> {
        if self.val.dims.len() != 1 || sub > self.val.dims[0] {
            return IE241.err_with(None, line);
        }
        self.val.elems[sub - 1] = val;
        Ok(())
    }

    pub fn get_md(&self, subs: &[usize], line: usize) -> Res<T> {
        let ix = self.get_index(subs, line)?;
        Ok(self.val.elems[ix])
    }

    #[allow(dead_code)]  // only used in compiled code
    pub fn get(&self, sub: usize, line: usize) -> Res<T>  {
        if self.val.dims.len() != 1 || sub > self.val.dims[0] {
            return IE241.err_with(None, line);
        }
        Ok(self.val.elems[sub - 1])
    }

    /// Helper to calculate an array index.
    fn get_index(&self, subs: &[usize], line: usize) -> Res<usize> {
        if subs.len() != self.val.dims.len() {
            return IE241.err_with(None, line);
        }
        let mut ix = 0;
        let mut prev_dim = 1;
        for (&sub, &dim) in subs.iter().zip(&self.val.dims) {
            if sub > dim {
                return IE241.err_with(None, line);
            }
            ix += (sub - 1) * prev_dim;
            prev_dim *= dim;
        }
        Ok(ix as usize)
    }

    pub fn dimension(&mut self, dims: Vec<usize>, line: usize) -> Res<()> {
        if dims.iter().product::<usize>() == 0 {
            return IE240.err_with(None, line);
        }
        if self.rw {
            self.val = Array::new(dims);
        }
        Ok(())
    }

    pub fn readout(&self, w: &mut dyn Write, state: &mut u8, line: usize) -> Res<()> {
        if self.val.dims.len() != 1 {
            // only dimension-1 arrays can be output
            return IE241.err_with(None, line);
        }
        let mut res = Vec::with_capacity(self.val.elems.len());
        for val in &self.val.elems {
            let byte = ((*state as i16 - val.to_u16() as i16) as u16 % 256) as u8;
            let mut c = byte;
            *state = byte as u8;
            c = (c & 0x0f) << 4 | (c & 0xf0) >> 4;
            c = (c & 0x33) << 2 | (c & 0xcc) >> 2;
            c = (c & 0x55) << 1 | (c & 0xaa) >> 1;
            res.push(c);
        }
        write_bytes(w, &res, line)
    }

    pub fn writein(&mut self, state: &mut u8, line: usize) -> Res<()> {
        if self.val.dims.len() != 1 {
            // only dimension-1 arrays can be input
            return IE241.err_with(None, line);
        }
        for place in &mut self.val.elems {
            let byte = read_byte();
            let c = if byte == 256 {
                *state = 0;
                256
            } else {
                let c = (byte as i16 - *state as i16) as u16 % 256;
                *state = byte as u8;
                c
            };
            if self.rw {
                *place = LikeU16::from_u16(c);
            }
        }
        Ok(())
    }
}

impl<T: Debug + Display> Display for Bind<T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{}", self.val)?;
        if !self.rw {
            write!(fmt, " RO")?;
        }
        if !self.stack.is_empty() {
            write!(fmt, " STACK={:?}", self.stack)?;
        }
        Ok(())
    }
}

impl<T: Debug> Display for Array<T> {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{:?}", self.elems)
    }
}

/// Generate a 32-bit random seed.
pub fn get_random_seed() -> u32 {
    let seed: u32;
    if let Ok(mut fp) = File::open("/dev/urandom") {
        let mut buf = [0; 4];
        let _ = fp.read(&mut buf);  // if no data, seed is just a little less random.
        seed = (buf[0] as u32) << 24 | (buf[1] as u32) << 16 | (buf[2] as u32) << 8 | (buf[3] as u32);
    } else {
        seed = 4;  // chosen by fair dice roll. guaranteed to be random.
    }
    seed
}

/// Check statement execution chance (false -> skip).
pub fn check_chance(chance: u8, state: &mut u32) -> bool {
    if chance == 100 {
        true
    } else {
        // this is the generator suggested as the default rand() by POSIX,
        // I'm sure it is exceedingly random
        *state = state.wrapping_mul(1103515245).wrapping_add(12345);
        let random = (*state / 65536) % 100;
        random < (chance as u32)
    }
}

/// Pop "n" jumps from the jump stack and return the last one.
pub fn pop_jumps<T>(jumps: &mut Vec<T>, n: u32, strict: bool, line: usize) -> Res<Option<T>> {
    if n == 0 {
        if strict {
            return IE621.err_with(None, line);
        } else {
            return Ok(None);
        }
    }
    if jumps.len() < n as usize {
        if strict {
            return IE632.err_with(None, line);
        } else {
            jumps.clear();
            return Ok(None);
        }
    }
    let newlen = jumps.len() - (n as usize - 1);
    jumps.truncate(newlen);
    Ok(jumps.pop())
}

/// Which roman digits from the `digit_tbl` to put together for each
/// decimal digit.
/// These are reversed because the whole digit string is reversed
/// in the end.
const ROMAN_TRANS_TBL: [&[usize]; 10] = [
    &[],           // 0
    &[0],          // 1
    &[0, 0],       // 2
    &[0, 0, 0],    // 3
    &[2, 1],       // 4   (alternate: &[0, 0, 0, 0])
    &[2],          // 5
    &[1, 2],       // 6
    &[1, 1, 2],    // 7
    &[1, 1, 1, 2], // 8
    &[3, 1]];      // 9

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

/// Convert a number into Roman numeral representation.
pub fn to_roman(mut val: u32) -> String {
    if val == 0 {
        // zero is just a lone overbar
        return "_\n\n".into();
    }
    let mut l1 = Vec::new();  // collect overbars
    let mut l2 = Vec::new();  // collect digits
    let mut place = 0;
    while val > 0 {
        let digit = (val % 10) as usize;
        for &idx in ROMAN_TRANS_TBL[digit] {
            l1.push(ROMAN_DIGIT_TBL[place][idx].0);
            l2.push(ROMAN_DIGIT_TBL[place][idx].1);
        }
        place += 1;
        val /= 10;
    }
    // NOTE: no itertools here, the file is copied to compilation input.
    format!("{}\n{}\n",
            l1.into_iter().rev().collect::<String>(),
            l2.into_iter().rev().collect::<String>())
}

const ENGLISH_DIGITS: [(&str, u64); 12] = [
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

/// Convert a number represented as digits spelled out in English.
pub fn from_english(v: &str, line: usize) -> Res<u32> {
    let mut res = 0;
    for word in v.split_whitespace() {
        match ENGLISH_DIGITS.iter().find(|entry| entry.0 == word) {
            Some(&(_, digit)) => res = 10*res + digit,
            None => return IE579.err_with(Some(word), line)
        }
    }
    if res > (u32::MAX as u64) {
        IE533.err_with(None, line)
    } else {
        Ok(res as u32)
    }
}

/// Output a number in Roman format.
pub fn write_number(w: &mut dyn Write, val: u32, line: usize) -> Res<()> {
    if write!(w, "{}", to_roman(val)).is_err() {
        return IE252.err_with(None, line);
    }
    Ok(())
}

/// Output a byte.
pub fn write_bytes(w: &mut dyn Write, val: &[u8], line: usize) -> Res<()> {
    if w.write_all(val).is_err() {
        return IE252.err_with(None, line);
    }
    Ok(())
}

/// Read a number in spelled out English format.
pub fn read_number(line: usize) -> Res<u32> {
    let mut buf = String::new();
    match stdin().read_line(&mut buf) {
        Ok(n) if n > 1 => from_english(&buf, line),
        _              => IE562.err_with(None, line)
    }
}

/// Read a byte from stdin.
pub fn read_byte() -> u16 {
    let mut buf = [0u8; 1];
    match stdin().read(&mut buf) {
        Ok(1) => buf[0] as u16,
        _     => 256      // EOF is defined to be 256
    }
}

/// Check for 16-bit overflow.
pub fn check_ovf(v: u32, line: usize) -> Res<u32> {
    if v > (u16::MAX as u32) {
        IE533.err_with(None, line)
    } else {
        Ok(v)
    }
}

/// Implements the Mingle operator.
pub fn mingle(mut v: u32, mut w: u32) -> u32 {
    v = ((v & 0x0000_ff00) << 8) | (v & 0x0000_00ff);
    v = ((v & 0x00f0_00f0) << 4) | (v & 0x000f_000f);
    v = ((v & 0x0c0c_0c0c) << 2) | (v & 0x0303_0303);
    v = ((v & 0x2222_2222) << 1) | (v & 0x1111_1111);
    w = ((w & 0x0000_ff00) << 8) | (w & 0x0000_00ff);
    w = ((w & 0x00f0_00f0) << 4) | (w & 0x000f_000f);
    w = ((w & 0x0c0c_0c0c) << 2) | (w & 0x0303_0303);
    w = ((w & 0x2222_2222) << 1) | (w & 0x1111_1111);
    (v << 1) | w
}

/// Implements the Select operator.
pub fn select(mut v: u32, mut w: u32) -> u32 {
    let mut i = 1;
    let mut t = 0;
    while w > 0 {
        if w & i > 0 {
            t |= v & i;
            w ^= i;
            i <<= 1;
        } else {
            w >>= 1;
            v >>= 1;
        }
    }
    t
}

pub fn and_16(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000;
    }
    w & v
}

pub fn and_32(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000_0000;
    }
    w & v
}

pub fn or_16(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000;
    }
    w | v
}

pub fn or_32(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000_0000;
    }
    w | v
}

pub fn xor_16(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000;
    }
    w ^ v
}

pub fn xor_32(v: u32) -> u32 {
    let mut w = v >> 1;
    if v & 1 > 0 {
        w |= 0x8000_0000;
    }
    w ^ v
}

pub trait LikeU16: Copy {
    fn from_u16(x: u16) -> Self;
    fn to_u16(self) -> u16;
}

impl LikeU16 for u16 {
    fn from_u16(x: u16) -> u16 { x }
    fn to_u16(self) -> u16 { self }
}

impl LikeU16 for u32 {
    fn from_u16(x: u16) -> u32 { x as u32 }
    fn to_u16(self) -> u16 { self as u16 }
}
