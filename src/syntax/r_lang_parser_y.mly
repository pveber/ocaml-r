%{
%}

%token <int> INT
%token <string> IDENT
%token SEMICOLON
%token ASSIGN EOL EOI

%start prog
%type <int list> prog

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
| expr eos { $1 }
;

expr:
| i = INT
    { i }
;


















