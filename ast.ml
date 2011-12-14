open Batteries;;
open Asttypes;;

let string_of_op op = 
    match op with
      PLUS  -> "plus"
    | MINUS -> "minus"
    | MUL   -> "mul"
    | DIV   -> "div"
    | POW   -> "pow"
;;

let string_of_uop op = 
    match op with
      UNARY_MINUS   -> "minus"
    | SIN           -> "sin"
;;


let rec traverse tree node_op leaf_op = 
    match tree with
      Leaf  l   -> leaf_op l
    | Ident i   -> node_op i
    | Node (lb, op, rb) ->
            node_op "(";
            traverse lb node_op leaf_op;
            traverse rb node_op leaf_op;
            node_op (string_of_op op);
            node_op ")";
    | UNode (b, op)     ->
            node_op "(";
            traverse b node_op leaf_op;
            node_op (string_of_uop  op);
            node_op ")";
;;

let rec calculate tree = 
    match tree with
      Leaf  l -> l
    | Ident i -> 1.0
    | Node (a, PLUS, b)     -> ( +. ) (calculate a) (calculate b)
    | Node (a, MINUS, b)    -> ( -. ) (calculate a) (calculate b)
    | Node (a, MUL, b)      -> ( *. ) (calculate a) (calculate b)
    | Node (a, DIV, b)      -> ( /. ) (calculate a) (calculate b)
    | Node (a, POW, b)      -> ( ** ) (calculate a) (calculate b)
    | UNode(a, UNARY_MINUS) -> -. (calculate a)
    | UNode(a, SIN)         -> sin (calculate a)
;;

let _ = 
    let lexbuf = Lexing.from_string "(sin (-2.0 * 1.0)) ^ 2.4 + x" in
    let result = Parser.main Lexer.token lexbuf in
(*    Printf.printf "%f\n" result*)
    let a  = Node ( Node (Leaf 2., PLUS, UNode(Leaf 2., SIN)), MUL, Node (Leaf 3., DIV, Leaf 3.)) in
    traverse result (Printf.printf "%s ") (Printf.printf "%f ");
    Printf.printf "\n";
(*    traverse a (Printf.printf " %s ") (Printf.printf " %f ");
    Printf.printf "\n";*)
 
(*    let b = calculate a in
    Printf.printf "%f\n" b*)
;;
