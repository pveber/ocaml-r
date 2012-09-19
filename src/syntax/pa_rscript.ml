open Camlp4.PreCast

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
  let i = R_lang_parser_y.prog R_lang_lexer.token buf in
  <:expr<0>>

let () = Quotation.(add "rscript" Quotation.DynAst.expr_tag) script_expander

















