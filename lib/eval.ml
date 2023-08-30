open Ast
open Object

(* TODO : switch return obj to error handling once eval is implemented *)

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
  | _ -> ERROR ("unknown operator: -(" ^ string_of_obj right ^ ")\n")
;;

let eval_prefix_expr op (right : obj) : obj =
  let open Token in
  match op with
  | BANG -> eval_bang_expr right
  | MINUS -> eval_minus_prefix_expr right
  | _ -> ERROR ("unexpected operator token: " ^ (string_of_token op) ^ "\n")
;;

let eval_int_infix_expr op (i1 : int) (i2 : int) : obj =
  let open Token in
  match op with
  | PLUS -> INT_OBJ (i1 + i2)
  | MINUS -> INT_OBJ (i1 - i2)
  | ASTERISK -> INT_OBJ (i1 * i2)
  | SLASH -> INT_OBJ (i1 / i2)
  | GT -> BOOL_OBJ (i1 > i2)
  | LT -> BOOL_OBJ (i1 < i2)
  | EQ -> BOOL_OBJ (i1 == i2)
  | NOT_EQ -> BOOL_OBJ (i1 != i2)
  | _ -> ERROR ("unexpected operator token: " ^ (string_of_token op) ^ "\n")
;;

let eval_bool_infix_expr op (b1 : bool) (b2 : bool) : obj =
  let open Token in
  match op with
  | EQ -> BOOL_OBJ (b1 == b2)
  | NOT_EQ -> BOOL_OBJ (b1 != b2)
  | _ -> ERROR ("unexpected operator token: " ^ (string_of_token op) ^ "\n")
;;

let eval_infix_expr op (left : obj) (right : obj) : obj =
  let open Token in
  match (left, right) with
  | (INT_OBJ i1, INT_OBJ i2) -> eval_int_infix_expr op i1 i2
  | (BOOL_OBJ b1, BOOL_OBJ b2) -> eval_bool_infix_expr op b1 b2
  | (BOOL_OBJ _, INT_OBJ _)
  | (INT_OBJ _, BOOL_OBJ _)
  | _ -> ERROR
    ("unknown operation: " 
    ^ (string_of_obj left)
    ^ " "
    ^ (string_of_token op)
    ^ " "
    ^ (string_of_obj right)
    ^ "\n")
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
  else match cond with
  | NULL -> NULL
  | _ -> eval_statement alt

and eval_expr (e : expression) : obj = 
  match e with
  | IntegerLiteral i -> INT_OBJ (i.value)
  | BooleanLiteral b -> BOOL_OBJ (b.value)
  | Prefix e -> let right = eval_expr e.right in
    eval_prefix_expr e.operator right
  | Infix e -> let left = eval_expr e.left in
    let right = eval_expr e.right in
    eval_infix_expr e.operator left right
  | If e -> let cond = eval_expr e.condition in 
    eval_if_expr cond e.consequence e.alternative
  | _ -> NULL

and eval_statement (s : statement) : obj = 
  match s with
  | ExpressionStatement e -> eval_expr e
  | Block sl -> eval_statements sl
  | Return e -> RETURN_OBJ (eval_expr e)
  | _ -> NULL

and eval_statements (sl : statement list) : obj = 
  match sl with
  | [] -> NULL
  | x::[] -> eval_statement x
  | x::xs -> let obj = eval_statement x in
    begin match obj with
    | RETURN_OBJ _ -> obj
    | _ -> eval_statements xs
    end
;;

let eval (ast : ast) = eval_statements ast.statements ;;

