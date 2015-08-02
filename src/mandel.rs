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

use std::io::{ Write, stdout };
use std::cmp::min;
use std::ops::{ Add, Mul };


#[derive(Clone, Copy, Debug)]
struct Complex(f64, f64);

impl Complex {
    pub fn abs(self) -> f64 {
        (self.0 * self.0 + self.1 * self.1).sqrt()
    }
}

impl Add for Complex {
    type Output = Complex;
    fn add(self, rhs: Complex) -> Complex {
        Complex(self.0 + rhs.0, self.1 + rhs.1)
    }
}

impl Mul for Complex {
    type Output = Complex;
    fn mul(self, rhs: Complex) -> Complex {
        Complex(self.0 * rhs.0 - self.1 * rhs.1,
                self.0 * rhs.1 + self.1 * rhs.0)
    }
}

fn generate_mandel(xpos: f64, ypos: f64, dist: f64) -> Vec<u16> {
    let width = 100;
    let height = 28;
    let factor = dist / width as f64;
    let xscale = factor * (1./3.);
    let yscale = factor;
    let xmin = xpos - xscale * width as f64 / 2.;
    let ymin = ypos - yscale * height as f64 / 2.;

    let mut res = vec![0; width * height];
    for iy in 0..height {
        for ix in 0..width {
            let c = Complex(xmin + xscale * ix as f64, ymin + yscale * iy as f64);
            let mut z = Complex(0., 0.);
            let mut color = 0;
            let mut mind = 2.;

            for _i in 0..170 {
                z = z * z + c;
                let d = z.abs();
                if d >= 2. {
                    color = min((mind / 0.007) as u16, 254) + 1;
                    break;
                } else {
                    mind = if d < mind { d } else { mind };
                }
            }

            res[iy * width + ix] = color;
        }
    }
    res
}

const LOCATIONS: [(f64, f64, f64, u16); 8] = [
    // x, y, "distance", max color range
    (-0.5, 0., 6.75, 256),
    (0.37865, 0.66923, 0.04, 111),
    (-1.2693, -0.4145, 0.2, 106),
    (-1.2693, -0.4145, 0.05, 97),
    (-1.2642, -0.4185, 0.01, 95),
    (-1.15, -0.28, 0.9, 99),
    (-1.15, -0.28, 0.3, 64),
    (-1.15, -0.28, 0.05, 29)];

const COLORS: [u8; 24] = [0xE8, 0xE9, 0xEA, 0xEB, 0xEC, 0xED, 0xEE, 0xEF,
                          0xF0, 0xF1, 0xF2, 0xF3, 0xF4, 0xF5, 0xF6, 0xF7,
                          0xF8, 0xF9, 0xFA, 0xFB, 0xFC, 0xFD, 0xFE, 0xFF];

pub struct MandelPrinter {
    location: usize,
    cur_vector: Vec<u16>,
    next_index: usize,
    max_color: u16,
}

impl MandelPrinter {
    pub fn new() -> MandelPrinter {
        MandelPrinter { cur_vector: vec![], location: 0, next_index: 0, max_color: 0 }
    }

    pub fn print_char(&mut self) {
        let idxmax = COLORS.len() as u16 - 1;
        let mut stdout = stdout();
        if self.next_index >= self.cur_vector.len() {
            let (x, y, dist, range) = LOCATIONS[self.location];
            self.max_color = range;
            self.cur_vector = generate_mandel(x, y, dist);
            self.next_index = 0;
            self.location = (self.location + 1) % LOCATIONS.len();
            println!("KEEP CALM AND STAND BY");
        }
        let color = self.cur_vector[self.next_index];
        if color > self.max_color {
            println!("{} {}", color, self.max_color);
        }
        let idx = idxmax - (color + 1) * idxmax / self.max_color;
        print!("\x1b[48;5;{}m \x1b[0m", COLORS[idx as usize]);
        let _ = stdout.flush();
        self.next_index += 1;
        if self.next_index % 100 == 0 {
            let _ = stdout.write(&['\n' as u8]);
        }
    }
}
