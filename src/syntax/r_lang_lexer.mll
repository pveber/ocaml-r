{
  open R_lang_parser_y
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

| eof
    { EOI }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }










