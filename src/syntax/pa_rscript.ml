open Printf
open Camlp4.PreCast

type script = {
  code : string ;
  wrapper : unit -> unit
}

let script_expander _loc _ s = 
  let buf = Lexing.from_string s in
  (* let _ =  *)
  (*   R_lang_parser_y.(try while true do  *)
  (*       match R_lang_lexer.token buf with *)
  (*       | EOI -> exit 0 *)
  (*       | INT _ -> prerr_endline "int" *)
  (*       | EOL -> prerr_endline "EOL" *)
  (*       | _ -> prerr_endline "biq" *)
  (*     done with _ -> ()) *)
  (* in  *)
  let ast = R_lang_parser_y.prog R_lang_lexer.token buf in
  let body = R_lang_ast.to_string ast in
  let code = sprintf "f <- function() {\n%s}\n" body in
  <:expr<
    let code = $str:code$ in 
    let _ = R.eval_string code in
    let stub = R.symbol "f" in
    R.eval stub []
  >>

let () = Quotation.(add "rscript" Quotation.DynAst.expr_tag) script_expander


















