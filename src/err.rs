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

//! Provides runtime errors.
//!
//! The term "runtime error" is actually a bit misleading: the errors produced by the
//! compiler and by the compiled program actually behave the same.  Certain error codes
//! can only be emitted by the compiler, and others only at program runtime.
//!
//! Most errors are accompanied by "ON THE WAY TO ..." followed by the source line
//! number of the following statement.  If the syslib or floatlib are automatically
//! appended to the program, their source line numbers will start where the original
//! program ended.
//!
//! In the interpreter, errors are usually constructed with line number 0, and the
//! interpreter sets the correct line number before it hands the error up to its
//! caller.  In compiled code, no such adjustment is done, so errors have to get the
//! correct line numbers when created.

use std::{fmt, io};

/// Result of a statement.
pub type Res<T> = Result<T, RtError>;

#[derive(PartialEq, Eq, Debug)]
pub struct ErrDesc {
    num: u16,
    msg: &'static str,
    way: Option<&'static str>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct RtError {
    error:  &'static ErrDesc,
    addstr: Option<String>,
    lineno: usize,
}

impl RtError {
    pub fn set_line(&mut self, lineno: usize) {
        self.lineno = lineno;
    }

    pub fn short_string(&self) -> &str {
        match self.addstr {
            Some(ref s) => s,
            None        => "???"
        }
    }

    pub fn to_code(&self) -> String {
        format!("err::IE{:03}.err_with({:?}, {:?})",
                self.error.num, self.addstr, self.lineno)
    }
}

impl fmt::Display for RtError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let mut msg = String::from(self.error.msg);
        if let Some(ref s) = self.addstr {
            msg = msg.replace("{}", s);
        }
        let lineinfo = match self.error.way {
            Some(s) => String::from(s),
            None => format!("ON THE WAY TO {}", self.lineno),
        };
        write!(f, "ICL{:03}I\t{}\n\t{}\
                   \n        CORRECT SOURCE AND RESUBNIT\n",
               self.error.num, msg, lineinfo)
    }
}

impl From<io::Error> for RtError {
    fn from(_: io::Error) -> RtError {
        IE888.mk(None, 0)
    }
}

impl ErrDesc {
    pub fn mk(&'static self, addstr: Option<String>, lineno: usize) -> RtError {
        RtError { error: self, addstr, lineno }
    }

    pub fn err<T>(&'static self) -> Result<T, RtError> {
        Err(RtError { error: self, addstr: None, lineno: 0 })
    }

    pub fn err_with<T>(&'static self, addstr: Option<&str>, lineno: usize) -> Result<T, RtError> {
        Err(RtError { error: self, addstr: addstr.map(|v| v.into()), lineno })
    }
}


/* An undecodable statement has been encountered in the course of execution. */
pub static IE000: ErrDesc = ErrDesc {
    num: 0,
    msg: "{}",
    way: None,
};

/* A numeric literal is out of range. */
pub static IE017: ErrDesc = ErrDesc {
    num: 17,
    msg: "DO YOU EXPECT ME TO FIGURE THIS OUT?",
    way: None,
};

/* Improper use has been made of statement identifiers. */
pub static IE079: ErrDesc = ErrDesc {
    num: 79,
    msg: "PROGRAMMER IS INSUFFICIENTLY POLITE",
    way: None,
};

/* Improper use has been made of statement identifiers. */
pub static IE099: ErrDesc = ErrDesc {
    num: 99,
    msg: "PROGRAMMER IS OVERLY POLITE",
    way: None,
};

/* Program has attempted 80 levels of NEXTing */
pub static IE123: ErrDesc = ErrDesc {
    num: 123,
    msg: "PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON",
    way: None,
};

/* Program has attempted to transfer to a non-existent line label */
pub static IE129: ErrDesc = ErrDesc {
    num: 129,
    msg: "PROGRAM HAS GOTTEN LOST",
    way: Some("ON THE WAY TO WHO KNOWS WHERE"),
};

/* An ABSTAIN or REINSTATE statement references a non-existent line label */
pub static IE139: ErrDesc = ErrDesc {
    num: 139,
    msg: "I WASN'T PLANNING TO GO THERE ANYWAY",
    way: None,
};

/* A line label has been multiply defined. */
pub static IE182: ErrDesc = ErrDesc {
    num: 182,
    msg: "YOU MUST LIKE THIS LABEL A LOT!",
    way: None,
};

/* An invalid line label has been encountered. */
pub static IE197: ErrDesc = ErrDesc {
    num: 197,
    msg: "SO!  65535 LABELS AREN'T ENOUGH FOR YOU?",
    way: None,
};

/* An expression involves an unidentified variable. */
pub static IE200: ErrDesc = ErrDesc {
    num: 200,
    msg: "NOTHING VENTURED, NOTHING GAINED",
    way: None,
};

/* An attempt has been made to give an array a dimension of zero. */
pub static IE240: ErrDesc = ErrDesc {
    num: 240,
    msg: "ERROR HANDLER PRINTED SNIDE REMARK",
    way: None,
};

/* Invalid dimensioning information was supplied
 * in defining or using an array. */
pub static IE241: ErrDesc = ErrDesc {
    num: 241,
    msg: "VARIABLES MAY NOT BE STORED IN WEST HYPERSPACE",
    way: None,
};

/* Could not write to the output stream. */
pub static IE252: ErrDesc = ErrDesc {
    num: 252,
    msg: "I'VE FORGOTTEN WHAT I WAS ABOUT TO SAY",
    way: None,
};

/* A 32-bit value has been assigned to a 16-bit variable. */
pub static IE275: ErrDesc = ErrDesc {
    num: 275,
    msg: "DON'T BYTE OFF MORE THAN YOU CAN CHEW",
    way: None,
};

/* A retrieval has been attempted for an unSTASHed value. */
pub static IE436: ErrDesc = ErrDesc {
    num: 436,
    msg: "THROW STICK BEFORE RETRIEVING!",
    way: None,
};

/* A COME FROM statement references a non-existent line label. */
pub static IE444: ErrDesc = ErrDesc {
    num: 444,
    msg: "IT CAME FROM BEYOND SPACE",
    way: None,
};

/* A WRITE IN statement or interleave ($) operation
 * has produced value requiring over 32 bits to represent. */
pub static IE533: ErrDesc = ErrDesc {
    num: 533,
    msg: "YOU WANT MAYBE WE SHOULD IMPLEMENT 64-BIT VARIABLES?",
    way: None,
};

/* More than one COME FROM references the same label. */
pub static IE555: ErrDesc = ErrDesc {
    num: 555,
    msg: "FLOW DIAGRAM IS EXCESSIVELY CONNECTED",
    way: None,
};

/* Insufficient data. (raised by reading past EOF) */
pub static IE562: ErrDesc = ErrDesc {
    num: 562,
    msg: "I DO NOT COMPUTE",
    way: None,
};

/* Input data is invalid. */
pub static IE579: ErrDesc = ErrDesc {
    num: 579,
    msg: "WHAT BASE AND/OR LANGUAGE INCLUDES {}?",
    way: None,
};

/* The expression of a RESUME statement evaluated to #0. */
pub static IE621: ErrDesc = ErrDesc {
    num: 621,
    msg: "ERROR TYPE 621 ENCOUNTERED",
    way: None,
};

/* Input data is invalid. */
pub static IE632: ErrDesc = ErrDesc {
    num: 632,
    msg: "THE NEXT STACK RUPTURES.  ALL DIE.  OH, THE EMBARRASSMENT!",
    way: None,
};

/* Execution has passed beyond the last statement of the program. */
pub static IE633: ErrDesc = ErrDesc {
    num: 633,
    msg: "PROGRAM FELL OFF THE EDGE",
    way: Some("ON THE WAY TO THE NEW WORLD"),
};

/* Error executing rustc. */
pub static IE666: ErrDesc = ErrDesc {
    num: 666,
    msg: "COMPILER HAS INDIGESTION",
    way: Some("ON THE WAY TO THE RESTROOM"),
};

/* A random compiler error has occurred. */
pub static IE774: ErrDesc = ErrDesc {
    num: 774,
    msg: "RANDOM COMPILER BUG",
    way: None,
};

/* No such source file. */
pub static IE777: ErrDesc = ErrDesc {
    num: 777,
    msg: "A SOURCE IS A SOURCE, OF COURSE, OF COURSE",
    way: None,
};

/* Can't open C output file. */
pub static IE888: ErrDesc = ErrDesc {
    num: 888,
    msg: "I HAVE NO FILE AND I MUST SCREAM",
    way: None,
};

/* Unknown invocation flag. */
pub static IE990: ErrDesc = ErrDesc {
    num: 990,
    msg: "FLAG ETIQUETTE FAILURE BAD SCOUT NO BISCUIT",
    way: None,
};

/* Command found after TRY AGAIN. */
pub static IE993: ErrDesc = ErrDesc {
    num: 993,
    msg: "I GAVE UP LONG AGO",
    way: None,
};

/* "Impossible" case in match, or other internal error. */
pub static IE994: ErrDesc = ErrDesc {
    num: 994,
    msg: "NOCTURNAL EMISSION, PLEASE LAUNDER SHEETS IMMEDIATELY",
    way: None,
};

/* Source file name with invalid extension (use .i). */
pub static IE998: ErrDesc = ErrDesc {
    num: 998,
    msg: "EXCUSE ME",
    way: Some("YOU MUST HAVE ME CONFUSED WITH SOME OTHER COMPILER"),
};
