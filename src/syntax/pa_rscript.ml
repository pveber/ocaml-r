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
  let body = R_lang_ast.to_string ast
  and arguments = R_lang_ast.free_variables ast in
  let oc = open_out "delme.R" in 
  fprintf oc "%s" body ;
  close_out oc ;
  let code =
    sprintf "f <- function(%s) {\n%s}\n"
      (String.concat "," (List.map (fun (x,_,_) -> x) arguments))
      body 
  and conversion_expr =
    List.fold_right
      (fun (var,typ,e) accu -> 
	let arg = match typ with
	  | `r -> <:expr< R.arg (fun x -> x) $e$>>
	  | `int -> <:expr< R.arg R.int $e$>>
	  | `string -> <:expr< R.arg R.string $e$>>
	  | `vector -> <:expr< R.arg R.floats $e$>>
	in 
	<:expr< [ $arg$ :: $accu$ ] >>)
      arguments <:expr< [] >>
  in
  <:expr<
    let code = $str:code$ in
    let _ = R.eval_string code in
    let stub = R.symbol "f" in
    R.eval stub $conversion_expr$
  >>

let () = Quotation.(add "rscript" Quotation.DynAst.expr_tag) script_expander


















