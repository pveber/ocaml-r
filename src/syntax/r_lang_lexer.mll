{
  open Camlp4.PreCast
  open Lexing
  open R_lang_parser_y

  (* Locating antiquotations *)
  let location lexbuf offset =
    let pos = lexeme_start_p lexbuf in
    let pos = { pos with pos_cnum = pos.pos_cnum + offset } in
    Loc.of_lexing_position pos
      
  let expr lexbuf offset text =
    Syntax.Gram.parse Syntax.expr_eoi (location lexbuf offset) (Stream.of_string text)


}

rule token = parse
| [' ''\t']+ { token lexbuf }
| '\n' { EOL }
| "<-" { ASSIGN }
| ';' { SEMICOLON }
| '(' { LPAREN }
| ')' { RPAREN }
| ',' { COMMA }
| '=' { EQUAL }

| ['0'-'9']+ as i
    { INT (int_of_string i) }

| ['A'-'Z''a'-'z''0'-'9''-''_']+ as lxm { IDENT(lxm) }

| '$' (([^ '?' '!' '$'] [^ ':']* as typ) ':' ([^'$']* as e)) '$'
    { EXPR (typ, expr lexbuf (2 + String.length typ) e) }

| eof
    { EOI }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }










