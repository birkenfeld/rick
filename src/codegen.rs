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

/// Translates AST to Rust.
///
/// There is quite a bit of ugly code generation here.  A library for quasi-quoting
/// and pretty-printing Rust code would help a lot here.  I tried, at least, to make
/// the actual generated code as nice as possible, so that it is not too hard to
/// see what is going on at runtime.
///
/// All of the INTERCAL statements are generated into one monstrous function with a
/// big switch over the current logical line (pctr is the "program counter").  This
/// function is called from main(), which does not much else.
///
/// A lot of the generated code is similar to what eval.rs does at runtime, but most
/// of the common code lives in stdops.rs.

use std::fs::File;
use std::io::{ BufWriter, Write };
use std::rc::Rc;
use std::u16;

use ast::{ Program, Stmt, StmtBody, Expr, Var, VType, Abstain, ComeFrom };
use err::{ Res, IE129, IE533, IE994 };
use lex::SrcLine;

const STDOPS_MOD_STR: &'static str = include_str!("stdops.rs");
const ERR_MOD_STR:    &'static str = include_str!("err.rs");


type WRes = Res<()>;  // write result, always unit

pub struct Generator {
    program: Rc<Program>,
    debug: bool,
    random: bool,
    o: BufWriter<File>,
    line: SrcLine,
}

/// An ad-hoc way to generate a newline followed by a certain amount of indentation.
fn indentation<'a>(n: usize) -> &'a str {
    &"
                                                                  "[..n+1]
}

macro_rules! w {
    ($o:expr, $i:expr; $s:expr, $($args:expr),+) => {
        try!(write!($o, concat!("{}", $s), indentation($i), $($args),+))
    };
    ($o:expr, $i:expr; $s:expr) => {
        try!(write!($o, concat!("{}", $s), indentation($i)))
    };
    ($o:expr; $s:expr, $($args:expr),+) => {
        try!(write!($o, $s, $($args),+))
    };
    ($o:expr; $s:expr) => {
        try!(write!($o, $s))
    };
}


impl Generator {
    pub fn new(program: Program, outfile: File, debug: bool, random: bool) -> Generator {
        Generator {
            program: Rc::new(program),
            debug: debug,
            random: random,
            o: BufWriter::new(outfile),
            line: 0,
        }
    }

    /// The main (and only) public method of the generator.
    pub fn generate(&mut self) -> WRes {
        let program = self.program.clone();
        try!(self.gen_attrs());
        try!(self.gen_stdmods());
        try!(self.gen_header());
        try!(self.gen_program_vars());
        try!(self.gen_loop_header());
        for (i, stmt) in program.stmts.iter().enumerate() {
            try!(self.gen_stmt_wrap(i, stmt));
        }
        try!(self.gen_loop_footer());
        try!(self.gen_footer());
        try!(self.o.flush());
        Ok(())
    }

    fn write(&mut self, s: &str) -> WRes {
        try!(self.o.write(s.as_bytes()));
        Ok(())
    }

    fn gen_attrs(&mut self) -> WRes {
        w!(self.o; "#![allow(unused_imports)]");
        Ok(())
    }

    fn gen_stdmods(&mut self) -> WRes {
        w!(self.o; "#[allow(dead_code)]\nmod err{{\n{}\n}}\n\n", ERR_MOD_STR);
        w!(self.o; "#[allow(dead_code)]\nmod stdops{{\n{}\n}}\n\n", STDOPS_MOD_STR);
        Ok(())
    }

    fn gen_stmt_wrap(&mut self, i: usize, stmt: &Stmt) -> WRes {
        self.line = stmt.props.onthewayto;
        // create a match arm
        w!(self.o, 12; "/* {} */", stmt);
        w!(self.o, 12; "{} => {{", i);
        if self.debug {
            w!(self.o, 16; "println!(\"{{}}\", \"{}\");", stmt);
        }
        // check abstention
        if stmt.can_abstain {
            w!(self.o, 16; "if abstain[{}] == 0 {{", i);
        } else {
            if stmt.props.disabled {
                w!(self.o, 16; "if false {{");
            } else {
                w!(self.o, 16; "{{");
            }
        }
        // check chance for statement execution
        if stmt.props.chance < 100 {
            w!(self.o, 18; "let (passed, new_rand_st) = check_chance({}, rand_st);",
               stmt.props.chance);
            w!(self.o, 18; "rand_st = new_rand_st;");
            w!(self.o, 18; "if passed {{");
        }
        try!(self.gen_stmt(stmt));
        // end of chance check
        if stmt.props.chance < 100 {
            w!(self.o, 18; "}}");
        }
        // end of abstain check
        w!(self.o, 16; "}}");
        // insert random compiler bug
        if i == self.program.bugline as usize {
            w!(self.o, 16; "return err::IE774.err_with(None, {});", self.line);
        }
        // COME FROM check
        if self.program.uses_complex_comefrom {
            let cand1 = if let Some(next) = stmt.comefrom {
                format!("Some({})", next)
            } else {
                format!("None")
            };
            let label = format!("{}", stmt.props.label);
            try!(self.gen_comefrom_check(&cand1, &label));
        } else if let Some(next) = stmt.comefrom {
            let chance = self.program.stmts[next as usize].props.chance;
            w!(self.o, 16; "if abstain[{}] == 0 {{   // COME FROM", next);
            if chance < 100 {
                w!(self.o, 18; "let (passed, new_rand_st) = check_chance({}, rand_st);",
                   chance);
                w!(self.o, 18; "rand_st = new_rand_st;");
                w!(self.o, 18; "if passed {{");
            }
            w!(self.o, 20; "pctr = {};", next);
            w!(self.o, 20; "continue;");
            if chance < 100 {
                w!(self.o, 18; "}}");
            }
            w!(self.o, 16; "}}");
        }
        // end of match arm
        w!(self.o, 16; "pctr += 1;");  // different from i + 1 after Resume
        w!(self.o, 12; "}}");
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> WRes {
        match stmt.body {
            StmtBody::DoNext(n) => {
                let next = match self.program.labels.get(&n) {
                    Some(i) => i,
                    None    => return IE129.err()
                };
                // Jumps are a bit problematic: when we resume, we'd need full
                // information about the current statement which is not available
                // at runtime.  Therefore we have to put the comefrom and the
                // label of the statement on the next stack as well.
                w!(self.o; "
                    if jumps.len() >= 80 {{
                        return err::IE123.err_with(None, {});
                    }}
                    jumps.push((pctr, {:?}, {}));
                    pctr = {};
                    continue;", self.program.stmts[*next as usize].props.srcline,
                   stmt.comefrom, stmt.props.label, next);
            }
            StmtBody::GiveUp => {
                w!(self.o, 20; "break;");
            }
            StmtBody::Error(ref e) => {
                w!(self.o, 20; "return {};", e.to_code());
            }
            StmtBody::ComeFrom(_) => {
                // nothing to do here
            }
            StmtBody::Dim(ref var, ref exprs) => {
                try!(self.gen_eval_subs(exprs));
                match *var {
                    Var::A16(n, _) => w!(self.o, 20; "try!(a{}.dimension(subs, {}));",
                                         n, self.line),
                    Var::A32(n, _) => w!(self.o, 20; "try!(b{}.dimension(subs, {}));",
                                         n, self.line),
                    _ => return IE994.err_with(None, self.line),
                }
            }
            StmtBody::Calc(ref var, ref expr) => {
                try!(self.gen_eval_expr(expr));
                try!(self.gen_assign(var));
            }
            StmtBody::Resume(ref expr) => {
                try!(self.gen_eval_expr(expr));
                w!(self.o, 20; "let (old_pctr, comefrom, label) = \
                   try!(pop_jumps(&mut jumps, val, true, {})).expect(\"uh oh\");", self.line);
                if self.program.uses_complex_comefrom {
                    try!(self.gen_comefrom_check("comefrom", "label"));
                } else {
                    // XXX: chance check missing here
                    w!(self.o, 20; "if let Some(next) = comefrom {{
                        if abstain[next] == 0 {{
                            pctr = next;
                            continue;
                        }}
                    }}");
                }
                w!(self.o, 20; "pctr = old_pctr + 1;
                    continue;");
            }
            StmtBody::Forget(ref expr) => {
                try!(self.gen_eval_expr(expr));
                w!(self.o, 20; "try!(pop_jumps(&mut jumps, val, false, {}));", self.line);
            }
            StmtBody::Ignore(ref vars) => {
                for var in vars {
                    w!(self.o, 20; "{}.rw = false;", Generator::get_varname(var));
                }
            }
            StmtBody::Remember(ref vars) => {
                for var in vars {
                    w!(self.o, 20; "{}.rw = true;", Generator::get_varname(var));
                }
            }
            StmtBody::Stash(ref vars) => {
                for var in vars {
                    w!(self.o, 20; "{}.stash();", Generator::get_varname(var));
                }
            }
            StmtBody::Retrieve(ref vars) => {
                for var in vars {
                    w!(self.o, 20; "try!({}.retrieve({}));",
                       Generator::get_varname(var), self.line);
                }
            }
            StmtBody::Abstain(ref expr, ref whats) => {
                let f: Box<Fn(String) -> String> = if let Some(ref e) = *expr {
                    try!(self.gen_eval_expr(e));
                    box |v| format!("{}.saturating_add(val)", v)
                } else {
                    box |_| format!("1")
                };
                for what in whats {
                    try!(self.gen_abstain(what, &*f));
                }
            }
            StmtBody::Reinstate(ref whats) => {
                w!(self.o, 20; "let val = 1;");
                for what in whats {
                    try!(self.gen_abstain(what, &|v| format!("{}.saturating_sub(1)", v)));
                }
            }
            StmtBody::ReadOut(ref exprs) => {
                for expr in exprs {
                    match *expr {
                        Expr::Var(ref var) if var.is_dim() => {
                            w!(self.o, 20; "try!({}.readout(&mut stdout, &mut last_out, {}));",
                               Generator::get_varname(var), self.line);
                        }
                        Expr::Var(_) => {
                            try!(self.gen_eval_expr(expr));
                            w!(self.o, 20; "try!(write_number(&mut stdout, val, {}));", self.line);
                        }
                        Expr::Num(_, v) => {
                            w!(self.o, 20; "try!(write_number(&mut stdout, {}, {}));", v, self.line);
                        }
                        _ => return IE994.err_with(None, self.line),
                    };
                }
            }
            StmtBody::WriteIn(ref vars) => {
                for var in vars {
                    if var.is_dim() {
                        w!(self.o, 20; "try!({}.writein(&mut last_in, {}));",
                           Generator::get_varname(var), self.line);
                    } else {
                        w!(self.o, 20; "let val = try!(read_number({}));",
                           self.line);
                        try!(self.gen_assign(var));
                    }
                }
            }
            StmtBody::TryAgain => {
                w!(self.o, 20; "pctr = 0;");
                w!(self.o, 20; "continue;");
            }
            StmtBody::Print(ref s) => {
                w!(self.o, 20; "if let Err(_) = stdout.write(&{:?}) {{", s);
                w!(self.o, 24; "return err::IE252.err_with(None, {})", self.line);
                w!(self.o, 20; "}}");
            }
        }
        Ok(())
    }

    /// Check for COME FROMs if the program uses computed COME FROM.
    fn gen_comefrom_check(&mut self, cand1: &str, label: &str) -> WRes {
        w!(self.o, 20; "let mut candidates = vec![];
                    if let Some(c) = {} {{ candidates.push(c); }}", cand1);
        let program = self.program.clone();
        for (i, stmt) in program.stmts.iter().enumerate() {
            if let StmtBody::ComeFrom(ComeFrom::Expr(ref e)) = stmt.body {
                try!(self.gen_eval_expr(e));
                w!(self.o, 20; "if val == {} && {} > 0 {{ candidates.push({}); }}",
                   label, label, i);
            }
        }
        w!(self.o, 20; "if candidates.len() > 1 {{ return err::IE555.err_with(None, {}); }}",
           self.line);
        // XXX: chance check missing here
        w!(self.o, 20; "if candidates.len() == 1 && abstain[candidates[0]] == 0 {{");
        w!(self.o, 24; "pctr = candidates[0];");
        w!(self.o, 24; "continue;");
        w!(self.o, 20; "}}");
        Ok(())
    }

    /// Get the Rust name of the given variable reference.
    fn get_varname(var: &Var) -> String {
        match *var {
            Var::I16(n) => format!("v{}", n),
            Var::I32(n) => format!("w{}", n),
            Var::A16(n, _) => format!("a{}", n),
            Var::A32(n, _) => format!("b{}", n),
        }
    }

    /// Generate an assignment of "val" to the given variable/array element.
    fn gen_assign(&mut self, var: &Var) -> WRes {
        // if the variable can't be IGNOREd, we can skip the check for it
        let suffix = if match *var {
            Var::I16(n) => self.program.var_info.0[n].can_ignore,
            Var::I32(n) => self.program.var_info.1[n].can_ignore,
            Var::A16(n, _) => self.program.var_info.2[n].can_ignore,
            Var::A32(n, _) => self.program.var_info.3[n].can_ignore,
        } { "" } else { "_unchecked" };
        match *var {
            Var::I16(n) => {
                // yes, we have to check this at this point - INTERCAL has no
                // real types, so the magnitude of the value is the only
                // reliable indicator whether we can put it into the variable
                w!(self.o; "
                    if val > (std::u16::MAX as u32) {{
                        return err::IE275.err_with(None, {});
                    }}", self.line);
                w!(self.o, 20; "v{}.assign{}(val as u16);", n, suffix);
            }
            Var::I32(n) => w!(self.o, 20; "w{}.assign{}(val);", n, suffix),
            Var::A16(n, ref subs) => {
                w!(self.o; "
                    if val > (std::u16::MAX as u32) {{
                        return err::IE275.err_with(None, {});
                    }}", self.line);
                if subs.len() == 1 {
                    w!(self.o, 20; "try!(a{}.set{}(", n, suffix);
                    try!(self.gen_eval(&subs[0], " as usize"));
                    w!(self.o; ", val as u16, {}));", self.line);
                } else {
                    try!(self.gen_eval_subs(subs));
                    w!(self.o, 20; "try!(a{}.set_md{}(subs, val as u16, {}));",
                       n, suffix, self.line);
                }
            }
            Var::A32(n, ref subs) => {
                if subs.len() == 1 {
                    w!(self.o, 20; "try!(b{}.set{}(", n, suffix);
                    try!(self.gen_eval(&subs[0], " as usize"));
                    w!(self.o; ", val, {}));", self.line);
                } else {
                    try!(self.gen_eval_subs(subs));
                    w!(self.o, 20; "try!(b{}.set_md{}(subs, val, {}));",
                       n, suffix, self.line);
                }
            }
        }
        Ok(())
    }

    /// Helper for ABSTAIN.
    fn gen_abstain(&mut self, what: &Abstain, gen: &Fn(String) -> String) -> WRes {
        if let &Abstain::Label(lbl) = what {
            let idx = self.program.labels[&lbl];
            w!(self.o, 20; "abstain[{}] = {};", idx, gen(format!("(abstain[{}] as u32)", idx)));
        } else {
            for (i, stype) in self.program.stmt_types.iter().enumerate() {
                if stype == what {
                    w!(self.o, 20; "abstain[{}] = {};", i, gen(format!("(abstain[{}] as u32)", i)));
                }
            }
        }
        Ok(())
    }

    /// Evaluate an expression and assign it to "val".
    fn gen_eval_expr(&mut self, expr: &Expr) -> WRes {
        w!(self.o, 20; "let val = ");
        try!(self.gen_eval(expr, ""));
        w!(self.o; ";");
        Ok(())
    }

    /// Evaluate a list of expressions and assign it to "subs".  Used for array subscriptions.
    fn gen_eval_subs(&mut self, exprs: &Vec<Expr>) -> WRes {
        w!(self.o, 20; "let subs = vec![");
        for (i, expr) in exprs.iter().enumerate() {
            try!(self.gen_eval(expr, " as usize"));
            if i < exprs.len() - 1 {
                w!(self.o; ", ");
            }
        }
        w!(self.o; "]; ");
        Ok(())
    }

    /// Evaluate an expression (inline).
    fn gen_eval(&mut self, expr: &Expr, astype: &str) -> WRes {
        match *expr {
            Expr::Num(_, v) => if v < 10 {
                w!(self.o; "{}", v);
            } else {
                w!(self.o; "{:#X}", v);
            },
            Expr::Var(ref var) => try!(self.gen_lookup(var, astype)),
            Expr::Mingle(ref vx, ref wx) => {
                w!(self.o; "mingle(");
                if let box Expr::Num(_, n) = *vx {
                    if n > (u16::MAX as u32) {
                        return IE533.err_with(None, self.line);
                    }
                    try!(self.gen_eval(vx, ""));
                } else {
                    w!(self.o; "try!(check_ovf(");
                    try!(self.gen_eval(vx, ""));
                    w!(self.o; ", {}))", self.line);
                }
                w!(self.o; ", ");
                if let box Expr::Num(_, n) = *wx {
                    if n > (u16::MAX as u32) {
                        return IE533.err_with(None, self.line);
                    }
                    try!(self.gen_eval(wx, ""));
                } else {
                    w!(self.o; "try!(check_ovf(");
                    try!(self.gen_eval(wx, ""));
                    w!(self.o; ", {}))", self.line);
                }
                w!(self.o; "){}", astype);
            }
            Expr::Select(vtype, ref vx, ref wx) => {
                w!(self.o; "select(");
                try!(self.gen_eval(vx, ""));
                w!(self.o; ", ");
                if vtype == VType::I16 {
                    w!(self.o; "try!(check_ovf(");
                    try!(self.gen_eval(wx, ""));
                    w!(self.o; ", {}))", self.line);
                } else {
                    try!(self.gen_eval(wx, ""));
                }
                w!(self.o; "){}", astype);
            }
            Expr::And(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "and_16("),
                    VType::I32 => w!(self.o; "and_32("),
                }
                try!(self.gen_eval(vx, ""));
                w!(self.o; "){}", astype);
            }
            Expr::Or(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "or_16("),
                    VType::I32 => w!(self.o; "or_32("),
                }
                try!(self.gen_eval(vx, ""));
                w!(self.o; "){}", astype);
            }
            Expr::Xor(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "xor_16("),
                    VType::I32 => w!(self.o; "xor_32("),
                }
                try!(self.gen_eval(vx, ""));
                w!(self.o; "){}", astype);
            }
            Expr::RsNot(ref vx) => {
                w!(self.o; "(!");
                try!(self.gen_eval(vx, ""));
                w!(self.o; "){}", astype);
            }
            Expr::RsAnd(ref vx, ref wx) => try!(self.gen_binop(vx, wx, "&", astype)),
            Expr::RsOr(ref vx, ref wx) => try!(self.gen_binop(vx, wx, "|", astype)),
            Expr::RsXor(ref vx, ref wx) => try!(self.gen_binop(vx, wx, "^", astype)),
            Expr::RsRshift(ref vx, ref wx) => try!(self.gen_binop(vx, wx, ">>", astype)),
            Expr::RsLshift(ref vx, ref wx) => try!(self.gen_binop_extrapar(vx, wx, "<<", astype)),
            // Expr::RsEqual(ref vx, ref wx) => try!(self.gen_binop(
            //     vx, wx, "==", if astype == "" { " as u32" } else { astype })),
            Expr::RsNotEqual(ref vx, ref wx) => try!(self.gen_binop(
                vx, wx, "!=", if astype == "" { " as u32" } else { astype })),
            Expr::RsPlus(ref vx, ref wx) => try!(self.gen_binop(vx, wx, "+", astype)),
            Expr::RsMinus(ref vx, ref wx) => try!(self.gen_binop(vx, wx, "-", astype)),
        }
        Ok(())
    }

    fn gen_binop(&mut self, vx: &Expr, wx: &Expr, op: &str, astype: &str) -> WRes {
        w!(self.o; "(");
        try!(self.gen_eval(vx, ""));
        w!(self.o; " {} ", op);
        try!(self.gen_eval(wx, ""));
        w!(self.o; "){}", astype);
        Ok(())
    }

    fn gen_binop_extrapar(&mut self, vx: &Expr, wx: &Expr, op: &str, astype: &str) -> WRes {
        w!(self.o; "((");
        try!(self.gen_eval(vx, ""));
        w!(self.o; ") {} (", op);
        try!(self.gen_eval(wx, ""));
        w!(self.o; ")){}", astype);
        Ok(())
    }

    /// Generate variable lookup inside an expression.
    fn gen_lookup(&mut self, var: &Var, astype: &str) -> WRes {
        match *var {
            Var::I16(n) => w!(self.o; "(v{}.val{})", n,
                              if astype == "" { " as u32" } else { astype }),
            Var::I32(n) => w!(self.o; "w{}.val{}", n, astype),
            Var::A16(n, ref subs) => {
                w!(self.o; "(try!(a{}.", n);
                if subs.len() == 1 {
                    w!(self.o; "get(");
                    try!(self.gen_eval(&subs[0], " as usize"));
                } else {
                    w!(self.o; "get_md(vec![");
                    for (i, expr) in subs.iter().enumerate() {
                        try!(self.gen_eval(expr, " as usize"));
                        if i < subs.len() - 1 {
                            w!(self.o; ", ");
                        }
                    }
                    w!(self.o; "]");
                }
                w!(self.o; ", {})){})", self.line, if astype == "" { " as u32" } else { astype });
            }
            Var::A32(n, ref subs) => {
                w!(self.o; "try!(b{}.", n);
                if subs.len() == 1 {
                    w!(self.o; "get(");
                    try!(self.gen_eval(&subs[0], " as usize"));
                } else {
                    w!(self.o; "get_md(vec![");
                    for (i, expr) in subs.iter().enumerate() {
                        try!(self.gen_eval(expr, " as usize"));
                        if i < subs.len() - 1 {
                            w!(self.o; ", ");
                        }
                    }
                    w!(self.o; "]");
                }
                w!(self.o; ", {})){}", self.line, astype);
            }
        }
        Ok(())
    }

    /// Generates local let-bindings for all the stuff we need to keep track of.
    fn gen_program_vars(&mut self) -> WRes {
        let vars = &self.program.var_info;
        // program counter
        w!(self.o, 4; "let mut pctr: usize = 0;");
        // output stream
        w!(self.o, 4; "let mut stdout = std::io::stdout();");
        // NEXT stack (80 entries only)
        w!(self.o, 4; "let mut jumps: Vec<(usize, Option<usize>, u16)> = Vec::with_capacity(80);");
        // current input and output state
        w!(self.o, 4; "let mut last_in: u8 = 0;");
        w!(self.o, 4; "let mut last_out: u8 = 0;");
        // random number generator state
        w!(self.o, 4; "let mut rand_st: u32;");
        // one binding for each variable used by the program
        for i in 0..vars.0.len() {
            w!(self.o, 4; "let mut v{}: Bind<u16> = Bind::new(0);", i);
        }
        for i in 0..vars.1.len() {
            w!(self.o, 4; "let mut w{}: Bind<u32> = Bind::new(0);", i);
        }
        for i in 0..vars.2.len() {
            w!(self.o, 4; "let mut a{}: Bind<Array<u16>> = Bind::new(Array::empty());", i);
        }
        for i in 0..vars.3.len() {
            w!(self.o, 4; "let mut b{}: Bind<Array<u32>> = Bind::new(Array::empty());", i);
        }
        // list of abstention state for each statement, can initially be 0 or 1
        w!(self.o, 4; "let mut abstain = [");
        for (i, stmt) in self.program.stmts.iter().enumerate() {
            if i % 24 == 0 {
                w!(self.o, 7; "");
            }
            w!(self.o; " {},", if stmt.props.disabled { "1" } else { "0" });
        }
        w!(self.o, 4; "];");
        Ok(())
    }

    fn gen_loop_header(&mut self) -> WRes {
        if self.random {
            w!(self.o, 4; "rand_st = get_random_seed();");
        } else {
            w!(self.o, 4; "rand_st = 0;");
        }
        self.write("
    loop {
        match pctr {")
    }

    fn gen_loop_footer(&mut self) -> WRes {
        w!(self.o, 12; "_ => {{");
        if let StmtBody::TryAgain = self.program.stmts[self.program.stmts.len() - 1].body {
            w!(self.o, 16; "break;");
        } else {
            w!(self.o, 16; "return err::IE633.err();");
        }
        self.write("
            }
        }
    }
    Ok(())")
    }

    fn gen_header(&mut self) -> WRes {
        self.write("
use std::io::Write;
use stdops::*;

#[allow(unused_mut, unused_parens, unused_variables, unused_assignments, unreachable_code)]
fn main_inner() -> err::Res<()> {")
    }

    fn gen_footer(&mut self) -> WRes {
        self.write("
}

fn main() {
    if let Err(err) = main_inner() {
        print!(\"{}\", err.to_string());
    }
}\n")
    }
}
