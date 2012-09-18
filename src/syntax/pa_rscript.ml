open Camlp4.PreCast

let script_expander _loc _ s = 
  let buf = Lexing.from_string s in
  let i = R_lang_parser_y.expr R_lang_lexer.token buf in
  <:expr<0>>

let () = Quotation.(add "rscript" Quotation.DynAst.expr_tag) script_expander

















