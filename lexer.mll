{
    open Parser;;
    exception Eof;;
}

rule token = parse
      [' ' '\t']                        { token lexbuf }
    | ['\n']                            { EOL }
    | ['0'-'9']+'.'['0'-'9']+ as lxm    { FLOAT(float_of_string lxm) }
    | ['x'-'z']['0'-'9']* as var        { IDENT(var) }
    | '+'                               { PLUS }
    | '-'                               { MINUS }
    | '*'                               { MUL }
    | '/'                               { DIV }
    | '^'                               { POW }
    | '('                               { LPAREN }
    | ')'                               { RPAREN }
    | "sin"                             { SIN }
    | eof				{ EOL }
