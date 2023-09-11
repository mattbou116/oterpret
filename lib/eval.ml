open Ast
open Object

(* TODO : switch return obj to error handling once eval is implemented *)

let true_literal = ref true;;
let false_literal = ref false;;
let null = ref NULL;;

let native_to_bool_obj (b : bool) : obj = 
  match b with
  | true -> BOOL_OBJ (!true_literal)
  | false -> BOOL_OBJ (!false_literal)
;;

let eval_bang_expr (right : obj) : obj =
  match right with
  | BOOL_OBJ true -> BOOL_OBJ false
  | BOOL_OBJ false -> BOOL_OBJ true
  | NULL -> BOOL_OBJ true
  | _ -> BOOL_OBJ false
;;

let eval_minus_prefix_expr (right : obj) : obj =
  match right with
  | INT_OBJ i -> INT_OBJ (-i)
  | _ -> ERROR 
    (Printf.sprintf "unknown operator: -(%s)" (string_of_obj_type right))
;;

let eval_prefix_expr op (right : obj) : obj =
  let open Token in
  match op with
  | BANG -> eval_bang_expr right
  | MINUS -> eval_minus_prefix_expr right
  | _ -> ERROR 
    (Printf.sprintf "unknown operator: %s(%s)" 
    (string_of_token op)
    (string_of_obj_type right))
;;

let eval_int_infix_expr op (i1 : int) (i2 : int) : obj =
  let open Token in
  match op with
  | PLUS -> INT_OBJ (i1 + i2)
  | MINUS -> INT_OBJ (i1 - i2)
  | ASTERISK -> INT_OBJ (i1 * i2)
  | SLASH -> INT_OBJ (i1 / i2)
  | GT -> native_to_bool_obj (i1 > i2)
  | LT -> native_to_bool_obj (i1 < i2)
  | EQ -> native_to_bool_obj (i1 == i2)
  | NOT_EQ -> native_to_bool_obj (i1 != i2)
  | _ -> ERROR 
    (Printf.sprintf "unknown operator: (%s %s %s)" 
    (string_of_obj_type @@ INT_OBJ (i1))
    (string_of_token op)
    (string_of_obj_type @@ INT_OBJ (i2)))
;;

let eval_bool_infix_expr op (b1 : bool) (b2 : bool) : obj =
  let open Token in
  match op with
  | EQ -> native_to_bool_obj (b1 == b2)
  | NOT_EQ -> native_to_bool_obj (b1 != b2)
  | _ -> ERROR 
    (Printf.sprintf "unknown operator: (%s %s %s)" 
    (string_of_obj_type @@ BOOL_OBJ (b1))
    (string_of_token op)
    (string_of_obj_type @@ BOOL_OBJ (b2)))
;;

let eval_infix_expr op (left : obj) (right : obj) : obj =
  let open Token in
  match (left, right) with
  | (INT_OBJ i1, INT_OBJ i2) -> eval_int_infix_expr op i1 i2
  | (BOOL_OBJ b1, BOOL_OBJ b2) -> eval_bool_infix_expr op b1 b2
  | _ -> ERROR
    (Printf.sprintf "unknown operation: (%s %s %s)" 
    (string_of_obj_type left)
    (string_of_token op)
    (string_of_obj_type right))
;;

let is_truthy (o : obj) : bool =
  match o with
  | BOOL_OBJ true -> true
  | BOOL_OBJ false -> false
  | NULL -> false
  | _ -> true
;;

let rec eval_if_expr cond (cons : statement) (alt : statement) =
  if (is_truthy cond) then
    eval_statement cons
  (*
  else match cond with
  | NULL -> !null
  | _ -> eval_statement alt
  *)
  else eval_statement alt

(** note : if eval call expression goes wrong, it's probably because 
    we are using eval_statements instead of a wrapper for 
    eval_block_statement (pg. 130)

    note : I added it bc p sure that we need it when we get to those
    damned call expressions
*)

and eval_block_statement (sl : statement list) : obj = 
  match sl with
  | [] -> !null
  | x::[] -> eval_statement x
  | x::xs -> let obj = eval_statement x in
    begin match obj with
    | RETURN_OBJ _ -> obj
    | ERROR _ -> obj
    | _ -> eval_block_statement xs
    end

and eval_expr (e : expression) : obj = 
  match e with
  | IntegerLiteral i -> INT_OBJ (i.value)
  | BooleanLiteral b -> native_to_bool_obj (b.value)
  | Prefix e -> 
      bind (eval_expr e.right) @@
        fun right -> eval_prefix_expr e.operator right
  | Infix e -> 
      bind (eval_expr e.left) @@
        fun left -> bind (eval_expr e.right) @@
          fun right -> eval_infix_expr e.operator left right
  | If e -> 
      bind (eval_expr e.condition) @@
      fun cond -> eval_if_expr cond e.consequence e.alternative
  | _ -> !null

and eval_statement (s : statement) : obj = 
  match s with
  | ExpressionStatement e -> eval_expr e
  | Block sl -> eval_block_statement sl
  | Return e -> bind (eval_expr e) @@
    fun o -> RETURN_OBJ o
  | _ -> !null

and eval_statements (sl : statement list) : obj = 
  match sl with
  | [] -> !null
  | x::[] -> eval_statement x
  | x::xs -> let obj = eval_statement x in
    begin match obj with
    | RETURN_OBJ o -> o
    | ERROR _ -> obj
    | _ -> eval_statements xs
    end
;;

let eval (ast : ast) : obj = eval_statements ast.statements ;;

