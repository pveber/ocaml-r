%token <int> INT
%token EOF

%start <int> expr

%%

expr:
| i = INT EOF
    { i }





















