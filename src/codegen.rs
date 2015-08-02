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

use std::fs::File;
use std::io::{ BufWriter, Write };
use std::rc::Rc;

use ast::{ Program, Stmt, StmtBody, Expr, Var, VType, Abstain };
use err::{ Res, IE129 };
use lex::SrcLine;

use err::MODULE_CODE_STR as ERR_MOD_STR;
use stdops::MODULE_CODE_STR as STDOPS_MOD_STR;


type WRes = Res<()>;  // write result, always unit

pub struct Generator {
    program: Rc<Program>,
    debug: bool,
    o: BufWriter<File>,
}

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
    pub fn new(program: Program, outfile: File, debug: bool) -> Generator {
        Generator {
            program: Rc::new(program),
            debug: debug,
            o: BufWriter::new(outfile),
        }
    }

    fn write(&mut self, s: &str) -> WRes {
        try!(self.o.write(s.as_bytes()));
        Ok(())
    }

    pub fn generate(&mut self) -> WRes {
        let program = self.program.clone();
        try!(self.gen_attrs());
        try!(self.gen_stdmods());
        try!(self.gen_header());
        try!(self.gen_program_vars(&*program));
        try!(self.gen_loop_header());
        for (i, stmt) in program.stmts.iter().enumerate() {
            try!(self.gen_stmt_wrap(i, stmt));
        }
        try!(self.gen_loop_footer());
        try!(self.gen_footer());
        try!(self.o.flush());
        Ok(())
    }

    fn gen_attrs(&mut self) -> WRes {
        // no attributes needed at the moment
        Ok(())
    }

    fn gen_stdmods(&mut self) -> WRes {
        w!(self.o; "#[allow(dead_code)]\n{}\n\n", ERR_MOD_STR);
        w!(self.o; "#[allow(dead_code)]\n{}\n\n", STDOPS_MOD_STR);
        Ok(())
    }

    fn gen_stmt_wrap(&mut self, i: usize, stmt: &Stmt) -> WRes {
        // create a match arm
        w!(self.o, 12; "/* {} */", stmt);
        w!(self.o, 12; "{} => {{", i);
        if self.debug {
            w!(self.o, 16; "println!(\"{{}}\", \"{}\");", stmt);
        }
        // check abstention
        w!(self.o, 16; "if !abstain[{}] {{", i);
        // check chance for statement execution
        if stmt.props.chance < 100 {
            w!(self.o, 18; "if check_chance({}) {{", stmt.props.chance);
        }
        try!(self.gen_stmt(stmt));
        // end of chance check
        if stmt.props.chance < 100 {
            w!(self.o, 18; "}}");
        }
        // end of abstain check
        w!(self.o, 16; "}}");
        // COME FROM check
        if let Some(next) = stmt.comefrom {
            w!(self.o; "
                // COME FROM
                if !abstain[{}] {{
                    pctr = {};
                    continue;
                }}", next, next);
        }
        // end of match arm
        w!(self.o, 16; "pctr += 1;");  // different from i + 1 after Resume
        w!(self.o, 12; "}}");
        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> WRes {
        let line = stmt.props.srcline;
        match stmt.body {
            StmtBody::DoNext(n) => {
                let next = match self.program.labels.get(&n) {
                    Some(i) => i,
                    None    => return IE129.err()
                };
                w!(self.o; "
                    if jumps.len() >= 80 {{
                        return err::IE123.err_with(None, {});
                    }}
                    jumps.push((pctr, {:?}));
                    pctr = {};
                    continue;", line, stmt.comefrom, next);
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
                try!(self.gen_eval_subs(exprs, line));
                match *var {
                    Var::A16(n, _) => w!(self.o, 20; "try!(a{}.dimension(subs));", n),
                    Var::A32(n, _) => w!(self.o, 20; "try!(b{}.dimension(subs));", n),
                    _ => unimplemented!()
                }
            }
            StmtBody::Calc(ref var, ref expr) => {
                try!(self.gen_eval_expr(expr, line));
                try!(self.gen_assign(var, line));
            }
            StmtBody::Resume(ref expr) => {
                try!(self.gen_eval_expr(expr, line));
                w!(self.o, 20; "let (old_pctr, comefrom) = \
                   try!(pop_jumps(&mut jumps, val, true)).unwrap();");
                w!(self.o, 20; "if let Some(next) = comefrom {{
                        if !abstain[next] {{
                            pctr = next;
                            continue;
                        }}
                    }}
                    pctr = old_pctr + 1;
                    continue;");
            }
            StmtBody::Forget(ref expr) => {
                try!(self.gen_eval_expr(expr, line));
                w!(self.o, 20; "try!(pop_jumps(&mut jumps, val, false));");
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
                    w!(self.o, 20; "try!({}.retrieve());", Generator::get_varname(var));
                }
            }
            StmtBody::Abstain(ref whats) => {
                for what in whats {
                    try!(self.gen_abstain(what, "true"));
                }
            }
            StmtBody::Reinstate(ref whats) => {
                for what in whats {
                    try!(self.gen_abstain(what, "false"));
                }
            }
            StmtBody::ReadOut(ref exprs) => {
                for expr in exprs {
                    match *expr {
                        Expr::Var(ref var) if var.is_dim() => {
                            w!(self.o, 20; "try!({}.readout(&mut last_out));",
                               Generator::get_varname(var));
                        }
                        Expr::Var(_) => {
                            try!(self.gen_eval_expr(expr, line));
                            w!(self.o, 20; "write_number(val);");
                        }
                        Expr::Num(_, v) => {
                            w!(self.o, 20; "write_number({})", v)
                        }
                        _ => unreachable!(),
                    };
                }
            }
            StmtBody::WriteIn(ref var) => {
                if var.is_dim() {
                    w!(self.o, 20; "try!({}.writein(&mut last_in));",
                       Generator::get_varname(var));
                } else {
                    w!(self.o, 20; "let val = try!(read_number());");
                    try!(self.gen_assign(var, line));
                }
            }
        }
        Ok(())
    }

    fn get_varname(var: &Var) -> String {
        match *var {
            Var::I16(n) => format!("v{}", n),
            Var::I32(n) => format!("w{}", n),
            Var::A16(n, _) => format!("a{}", n),
            Var::A32(n, _) => format!("b{}", n),
        }
    }

    fn gen_assign(&mut self, var: &Var, line: SrcLine) -> WRes {
        let suffix = if match *var {
            Var::I16(n) => self.program.var_info.0[n].can_ignore,
            Var::I32(n) => self.program.var_info.1[n].can_ignore,
            Var::A16(n, _) => self.program.var_info.2[n].can_ignore,
            Var::A32(n, _) => self.program.var_info.3[n].can_ignore,
        } { "" } else { "_unchecked" };
        match *var {
            Var::I16(n) => w!(self.o; "
                    if val > (u16::MAX as u32) {{
                        return err::IE275.err_with(None, {});
                    }}
                    v{}.assign{}(val as u16);", line, n, suffix),
            Var::I32(n) => w!(self.o, 20; "w{}.assign{}(val);", n, suffix),
            Var::A16(n, ref subs) => {
                try!(self.gen_eval_subs(subs, line));
                w!(self.o; "
                    if val > (u16::MAX as u32) {{
                        return err::IE275.err_with(None, {});
                    }}
                    try!(a{}.arr_assign{}(subs, val as u16));", line, n, suffix);
            }
            Var::A32(n, ref subs) => {
                try!(self.gen_eval_subs(subs, line));
                w!(self.o, 20; "try!(b{}.arr_assign{}(subs, val));", n, suffix);
            }
        }
        Ok(())
    }

    fn gen_abstain(&mut self, what: &Abstain, yesno: &str) -> WRes {
        if let &Abstain::Label(lbl) = what {
            let idx = self.program.labels[&lbl];
            w!(self.o, 20; "abstain[{}] = {};", idx, yesno);
        } else {
            for (i, stype) in self.program.stmt_types.iter().enumerate() {
                if stype == what {
                    w!(self.o, 20; "abstain[{}] = {};", i, yesno);
                }
            }
        }
        Ok(())
    }

    fn gen_eval_expr(&mut self, expr: &Expr, line: SrcLine) -> WRes {
        w!(self.o, 20; "let val = ");
        try!(self.gen_eval(expr, line));
        w!(self.o; ";");
        Ok(())
    }

    fn gen_eval_subs(&mut self, exprs: &Vec<Expr>, line: SrcLine) -> WRes {
        w!(self.o, 20; "let subs = vec![");
        for expr in exprs {
            try!(self.gen_eval(expr, line));
            w!(self.o; " as usize, ");
        }
        w!(self.o; "]; ");
        Ok(())
    }

    fn gen_eval(&mut self, expr: &Expr, line: SrcLine) -> WRes {
        match *expr {
            Expr::Num(_, v) => w!(self.o; "{}", v),
            Expr::Var(ref var) => try!(self.gen_lookup(var, line)),
            Expr::Mingle(ref vx, ref wx) => {
                w!(self.o; "mingle(try!(check_ovf(");
                try!(self.gen_eval(vx, line));
                w!(self.o; ", {})), try!(check_ovf(", line);
                try!(self.gen_eval(wx, line));
                w!(self.o; ", {})))", line);
            }
            Expr::Select(ref vx, ref wx) => {
                w!(self.o; "select((");
                try!(self.gen_eval(vx, line));
                w!(self.o; ") as u32, (");
                try!(self.gen_eval(wx, line));
                w!(self.o; ") as u32)");
            }
            Expr::And(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "and_16("),
                    VType::I32 => w!(self.o; "and_32("),
                }
                try!(self.gen_eval(vx, line));
                w!(self.o; ")");
            }
            Expr::Or(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "or_16("),
                    VType::I32 => w!(self.o; "or_32("),
                }
                try!(self.gen_eval(vx, line));
                w!(self.o; ")");
            }
            Expr::Xor(vtype, ref vx) => {
                match vtype {
                    VType::I16 => w!(self.o; "xor_16("),
                    VType::I32 => w!(self.o; "xor_32("),
                }
                try!(self.gen_eval(vx, line));
                w!(self.o; ")");
            }
        }
        Ok(())
    }

    fn gen_lookup(&mut self, var: &Var, line: SrcLine) -> WRes {
        match *var {
            Var::I16(n) => w!(self.o; "v{}.val as u32", n),
            Var::I32(n) => w!(self.o; "w{}.val", n),
            Var::A16(n, ref subs) => {
                w!(self.o; "try!(a{}.arr_lookup(vec![", n);
                for expr in subs {
                    try!(self.gen_eval(expr, line));
                    w!(self.o; " as usize, ");
                }
                w!(self.o; "])) as u32");
            }
            Var::A32(n, ref subs) => {
                w!(self.o; "try!(b{}.arr_lookup(vec![", n);
                for expr in subs {
                    try!(self.gen_eval(expr, line));
                    w!(self.o; " as usize, ");
                }
                w!(self.o; "]))");
            }
        }
        Ok(())
    }

    fn gen_program_vars(&mut self, program: &Program) -> WRes {
        let vars = &self.program.var_info;
        w!(self.o, 4; "let mut pctr: usize = 0;");
        w!(self.o, 4; "let mut jumps: Vec<(usize, Option<usize>)> = Vec::with_capacity(80);");
        w!(self.o, 4; "let mut last_in: u8 = 0;");
        w!(self.o, 4; "let mut last_out: u8 = 0;");
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
        w!(self.o, 4; "let mut abstain = [");
        for stmt in &program.stmts {
            w!(self.o, 8; "{},", if stmt.props.disabled { "true" } else { "false" });
        }
        w!(self.o, 4; "];");
        Ok(())
    }

    fn gen_loop_header(&mut self) -> WRes {
        self.write("
    loop {
        match pctr {")
    }

    fn gen_loop_footer(&mut self) -> WRes {
        self.write("
            n => {
                return err::IE663.err_with(None, n);
            }
        }
    }
    Ok(())")
    }

    fn gen_header(&mut self) -> WRes {
        self.write("
use std::u16;

use stdops::*;

#[allow(unused_mut, unused_variables)]
fn main_inner() -> err::Res<()> {
    seed_chance();")
    }

    fn gen_footer(&mut self) -> WRes {
        self.write("
}

fn main() {
    if let Err(err) = main_inner() {
        println!(\"{}\", err.to_string());
    }
}\n")
    }
}
