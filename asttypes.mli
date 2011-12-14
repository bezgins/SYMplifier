type operation =
      PLUS
    | MINUS
    | MUL
    | DIV
    | POW
;;

type unary_op = 
      UNARY_MINUS
    | SIN
;;

type ast =
      Leaf of float
    | Ident of string
    | Node of (ast * operation * ast)
    | UNode of (ast * unary_op)
;;

