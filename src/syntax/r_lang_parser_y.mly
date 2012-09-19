%{
  open R_lang_ast
%}

%token <int> INT
%token <string> IDENT
%token SEMICOLON
%token ASSIGN EOL EOI

%start prog
%type <R_lang_ast.t> prog

%%

prog:
| statements EOI { List.rev $1 }
;

statements:
| 
    { [] }
| statements EOL { $1 }
| statements statement { $2 :: $1 }
;

eos:
| SEMICOLON { () }
| EOL { () }
;

statement:
| expr eos { St_expr $1 }
;

expr:
| i = INT
    { Expr_int i }
;


















