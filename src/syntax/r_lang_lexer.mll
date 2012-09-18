{
  open R_lang_parser_y
}

rule token = parse
| ' ' '\t' 
    { token lexbuf }

| ['0'-'9']+ as i
    { INT (int_of_string i) }

| eof
    { EOF }

| _
    { failwith (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf)) }
