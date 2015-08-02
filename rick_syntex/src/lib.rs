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

#![feature(plugin_registrar, rustc_private, quote)]

extern crate rustc;
extern crate syntax;

use rustc::plugin::Registry;

use syntax::ptr::P;
use syntax::ast::{ Ident, Item_, MetaItem, Lit_, StrStyle, Visibility };
use syntax::codemap::{ Span, DUMMY_SP };
use syntax::ext::build::AstBuilder;
use syntax::ext::base::{ Annotatable, ExtCtxt, SyntaxExtension };
use syntax::parse::token::{ intern, intern_and_get_ident };
use syntax::print::pprust;

#[plugin_registrar]
pub fn registrar(reg: &mut Registry) {
    reg.register_syntax_extension(
        intern("rick_embed_module_code"),
        SyntaxExtension::MultiModifier(Box::new(add_mod_str)));
}

fn add_mod_str(cx: &mut ExtCtxt, span: Span, _: &MetaItem, item: Annotatable)
               -> Annotatable {
    if let Annotatable::Item(ref item) = item {
        if let Item_::ItemMod(_) = (*item).node {
            let str_ty = quote_ty!(cx, &'static str);
            // pretty-print the module code as a string
            let code_str = pprust::item_to_string(&item);
            // create a literal expression for the string
            let str_expr = cx.expr_lit(
                DUMMY_SP,
                Lit_::LitStr(intern_and_get_ident(&code_str),
                             StrStyle::CookedStr));
            // create a const item
            let str_item = cx.item_const(
                DUMMY_SP,
                Ident::new(intern("MODULE_CODE_STR")),
                str_ty, str_expr);
            // make it public
            let str_item = str_item.map(
                |mut i| { i.vis = Visibility::Public; i });
            // append it to the module's items
            let mut new_item = (**item).clone();
            match new_item.node {
                Item_::ItemMod(ref mut module) => module.items.push(str_item),
                _                              => unreachable!()
            }
            // return the new item
            return Annotatable::Item(P(new_item));
        }
    }
    cx.span_fatal(span, "rick_embed_module_code can only apply to modules")
}
