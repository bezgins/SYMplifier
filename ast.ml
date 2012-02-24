open Batteries;;
open Asttypes;;

(* Текстовое описание операции *)
let string_of_op op = 
    match op with
      PLUS  -> "plus"
    | MINUS -> "minus"
    | MUL   -> "mul"
    | DIV   -> "div"
    | POW   -> "pow"
;;

(* Символьное описание операции *)
let name_of_op op = 
    match op with
      PLUS  -> "+"
    | MINUS -> "-"
    | MUL   -> "*"
    | DIV   -> "/"
    | POW   -> "^"
;;

(* Предыдущие пункты, только для унарных операций *)
let string_of_uop op = 
    match op with
      UNARY_MINUS   -> "minus"
    | SIN           -> "sin"
;;

let name_of_uop op =
    match op with
      UNARY_MINUS   -> "-"
    | SIN           -> "sin"
;;

(* Рекурсивное формирование вывода для graphviz *)
let rec traverse_gv' tree first n =
    (*  * Получаем описание ноды:
        * кортеж (имя, надпись, стиль)
        * *)
    let get_node tree' n' =
        match tree' with
          Leaf  l           ->
                (Printf.sprintf "%.2f%d" l n', Printf.sprintf "%.2f" l, "diamond")
        | Ident i           ->
                (Printf.sprintf "%s%d" i n', i, "box")
        | Node  (_, op, _)  ->
                let op_str  = string_of_op op 
                and op_name = name_of_op op in
                (Printf.sprintf "%s%d" op_str n', op_name, "ellipse")
        | UNode (_, op)     ->
                let op_str  = string_of_uop op
                and op_name = name_of_uop op in
                (Printf.sprintf "%s%d" op_str n', op_name, "triangle")
    in
    match tree with
      (* Для  чисел и идентификаторов ничего не пишем*)
      Leaf _ | Ident _  ->
            ""
      (* Для бинарных операторов *)
    | Node (a, op, b)   ->
            (* получаем описания детей *)
            let (left_name, left_val, left_st)   = get_node a (!n+1)
            and (right_name, right_val, right_st) = get_node b (!n+2) in
            let _ = (n := !n+2) in
            let left_str  = (traverse_gv' a left_name n) 
            and right_str = (traverse_gv' b right_name n) in
            (* печатаем ноды детей и связи*)
            (Printf.sprintf "%s [label=\"%s\" shape=%s]\n"
                            left_name left_val left_st)^ 
            (Printf.sprintf "%s [label=\"%s\" shape=%s]\n"
                            right_name right_val right_st) ^
            (Printf.sprintf "{rank=same %s %s}\n"
                            left_name right_name) ^
            (Printf.sprintf "%s -- %s "
                            first left_name) ^
            (Printf.sprintf "%s -- %s "
                            first right_name) ^
            (Printf.sprintf "%s -- %s [style=invis]\n\n"
                            left_name right_name) ^
            (* Рекурсивно спускаемся на уровень ниже *)
            left_str ^
            right_str
      (* Унарные операторы *)
    | UNode (a, op)     ->
            (* описание ребенка *)
            let (left_name, left_val, left_st)   = get_node a (!n+1) in
            let _ = (n:=!n+1) in
            let child_str = (traverse_gv' a left_name n) in
            (* напечатаем детей и связи*)
            (Printf.sprintf "%s [label=\"%s\" shape=%s]\n"
                            left_name left_val left_st)^ 
            (Printf.sprintf "%s -- %s \n\n"
                            first left_name) ^
            (* рекурсивно спускаемся ниже *)
            child_str
;;


(* Формирование вывода для graphviz *)
let traverse_gv tree name = 
    (* Шапка и подвал *)
    let header = Printf.sprintf "graph gr {\n label=\"%s\"\n" name
    and footer = "}\n"
    (* Получим описание верхней ноды *)
    and (first_name, first_val) =
        match tree with
          Leaf  l           ->
                let l_name = Printf.sprintf "%.2f" l in
                (l_name, l_name)
        | Ident i           ->
                (i, i)
        | Node  (_, op, _)  ->
                let op_str  = string_of_op op 
                and op_name = name_of_op op in
                (op_str, op_name)
        | UNode (_, op)     ->
                let op_str  = string_of_uop op
                and op_name = name_of_uop op in
                (op_str, op_name)
    in
    (* Напечатаем шапка - первая нода - все остальное - подвал*)
    let first = Printf.sprintf "%s [label=\"%s\"]\n" first_name first_val
    and n = ref 1 in
    let rest  = traverse_gv' tree first_name n in
    header^first^rest^footer
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

(* Реализация распределительного закона *)
let rec distribute tree = 
    match tree with
    (* если выражение вида a*(b+c) или (b+c)*a *)
      Node (               a, MUL , Node (b, PLUS, c)) 
    | Node (Node(b, PLUS, c), MUL , a) -> 
          (* рекурсивно применим закон к слагаемым и сомножителю *)
          let a' = distribute a
          and b' = distribute b
          and c' = distribute c in
          (* И запишем в виде a*b + a*c *)
          distribute
          (Node(
              Node (a', MUL, b'),
              PLUS,
              Node (a', MUL, c')))
    (* тоже самое с вычитанием *)
    | Node (                a, MUL, Node (b, MINUS, c))
    | Node (Node(b, MINUS, c), MUL, a)-> 
          let a' = distribute a
          and b' = distribute b
          and c' = distribute c in
          distribute
          (Node(
              Node (a', MUL, b'),
              MINUS,
              Node (a', MUL, c')))
    (* Если другой оператор, то применим закон к аргументам *)
    | Node (a, op, b) -> 
            let a' = distribute a
            and b' = distribute b in
            Node (a', op, b')
    (* Если это вообще не оператор, то не трогаем *)
    | t -> t
;;

let _ = 
    let in_file = Pervasives.open_in "input.txt" in
    let input = Pervasives.input_line in_file in
    let lexbuf = Lexing.from_string input in
    let result = Parser.main Lexer.token lexbuf in
    let result' = distribute result in
    print_string (traverse_gv result' input);
;;
