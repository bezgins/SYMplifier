%{
open Asttypes;;
%}

%token <float> FLOAT
%token <string> IDENT
%token PLUS MINUS MUL DIV POW
%token LPAREN RPAREN
%token EOL
%token SIN

%left PLUS MINUS
%left MUL DIV POW

%nonassoc UMINUS
%nonassoc USIN

%start main
%type <Asttypes.ast> main
%%

main:
    expr EOL        { $1 }
;

expr:
      FLOAT                 { Leaf $1 }
    | IDENT                 { Ident $1 }
    | LPAREN expr RPAREN    { $2 }
    | expr PLUS expr        { Node ($1, PLUS, $3) }
    | expr MINUS expr       { Node ($1, MINUS, $3) }
    | expr MUL expr         { Node ($1, MUL, $3) }
    | expr DIV expr         { Node ($1, DIV, $3) }
    | expr POW expr         { Node ($1, POW, $3) }
    | MINUS expr %prec UMINUS { UNode ($2, UNARY_MINUS) }
    | SIN expr %prec USIN   { UNode ($2, SIN) }
;

