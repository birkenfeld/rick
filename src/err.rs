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


#[derive(PartialEq, Eq, Debug)]
pub struct ErrDesc {
    num: u16,
    msg: &'static str,
    way: Option<&'static str>,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error {
    error:  &'static ErrDesc,
    addstr: Option<String>,
    lineno: usize,
}

impl Error {
    pub fn set_line(&mut self, lineno: usize) {
        self.lineno = lineno;
    }

    pub fn to_string(&self) -> String {
        let mut msg = String::from(self.error.msg);
        if let Some(ref s) = self.addstr {
            msg = msg.replace("{}", &s);
        }
        let lineno = match self.error.way {
            Some(s) => String::from(s),
            None => format!("{}", self.lineno + 1),  // "on the way to..."
        };
        format!("ICL{:03}I {}\n        ON THE WAY TO {}\
                 \n        CORRECT SOURCE AND RESUBNIT\n",
                self.error.num, msg, lineno)
    }

    pub fn short_string(&self) -> &str {
        match self.addstr {
            Some(ref s) => s,
            None        => "???"
        }
    }
}

pub fn full(desc: &'static ErrDesc, addstr: Option<String>, line: usize) -> Error {
    Error { error: desc,
            addstr: addstr,
            lineno: line }
}

pub fn with_line(desc: &'static ErrDesc, line: usize) -> Error {
    Error { error: desc,
            addstr: None,
            lineno: line }
}

pub fn with_str(desc: &'static ErrDesc, addstr: &str) -> Error {
    Error { error: desc,
            addstr: Some(addstr.into()),
            lineno: 0 }
}

pub fn new(desc: &'static ErrDesc) -> Error {
    Error { error: desc,
            addstr: None,
            lineno: 0 }
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

/* Program has attempted 80 levels of NEXTing */
pub static IE123: ErrDesc = ErrDesc {
    num: 123,
    msg: "PROGRAM HAS DISAPPEARED INTO THE BLACK LAGOON",
    way: Some("WHO KNOWS WHERE"),
};

/* Program has attempted to transfer to a non-existent line label */
pub static IE129: ErrDesc = ErrDesc {
    num: 129,
    msg: "PROGRAM HAS GOTTEN LOST",
    way: Some("WHO KNOWS WHERE"),
};

/* An ABSTAIN or REINSTATE statement references a non-existent line label */
pub static IE139: ErrDesc = ErrDesc {
    num: 139,
    msg: "I WASN'T PLANNING TO GO THERE ANYWAY",
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

/* Invalid dimensioning information was supplied
 * in defining or using an array. */
pub static IE241: ErrDesc = ErrDesc {
    num: 241,
    msg: "VARIABLES MAY NOT BE STORED IN WEST HYPERSPACE",
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
pub static IE663: ErrDesc = ErrDesc {
    num: 663,
    msg: "PROGRAM FELL OFF THE EDGE",
    way: Some("THE NEW WORLD"),
};

/* Unimplemented feature used. This should never come up, hopefully. */
// pub static IE995: ErrDesc = ErrDesc {
//     num: 995,
//     msg: "DO YOU REALLY EXPECT ME TO HAVE IMPLEMENTED THAT?",
//     way: None,
// };
