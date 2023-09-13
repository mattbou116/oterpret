open Ast
open Object
open Environment

(* TODO : switch return obj to error handling once eval is implemented *)

let true_literal = ref true
let false_literal = ref false
let null = ref NULL

let native_to_bool_obj (b : bool) : obj =
  match b with
  | true -> BOOL_OBJ !true_literal
  | false -> BOOL_OBJ !false_literal
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
  | _ ->
    ERROR (Printf.sprintf "unknown operator: -(%s)" (string_of_obj_type right))
;;

let eval_prefix_expr op (right : obj) : obj =
  let open Token in
  match op with
  | BANG -> eval_bang_expr right
  | MINUS -> eval_minus_prefix_expr right
  | _ ->
    ERROR
      (Printf.sprintf
         "unknown operator: %s(%s)"
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
  | _ ->
    ERROR
      (Printf.sprintf
         "unknown operator: (%s %s %s)"
         (string_of_obj_type @@ INT_OBJ i1)
         (string_of_token op)
         (string_of_obj_type @@ INT_OBJ i2))
;;

let eval_bool_infix_expr op (b1 : bool) (b2 : bool) : obj =
  let open Token in
  match op with
  | EQ -> native_to_bool_obj (b1 == b2)
  | NOT_EQ -> native_to_bool_obj (b1 != b2)
  | _ ->
    ERROR
      (Printf.sprintf
         "unknown operator: (%s %s %s)"
         (string_of_obj_type @@ BOOL_OBJ b1)
         (string_of_token op)
         (string_of_obj_type @@ BOOL_OBJ b2))
;;

let eval_infix_expr op (left : obj) (right : obj) : obj =
  let open Token in
  match left, right with
  | INT_OBJ i1, INT_OBJ i2 -> eval_int_infix_expr op i1 i2
  | BOOL_OBJ b1, BOOL_OBJ b2 -> eval_bool_infix_expr op b1 b2
  | _ ->
    ERROR
      (Printf.sprintf
         "unknown operation: (%s %s %s)"
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

let eval_identifier (i : identifier) (env : obj Env.t) : obj =
  match Env.find_opt i.ident env with
  | None -> ERROR (Printf.sprintf "identifier not found: (%s)" i.ident)
  | Some v -> v
;;

let rec eval_if_expr cond (cons : statement) (alt : statement) (env : obj Env.t)
  =
  if is_truthy cond
  then (
    let obj, _ = eval_statement cons env in
    obj)
  else (
    let obj, _ = eval_statement alt env in
    obj)

and eval_block_statement (sl : statement list) (env : obj Env.t)
  : obj * obj Env.t
  =
  let rec aux sl env obj =
    match sl with
    | [] -> obj, env
    | x :: xs ->
      let obj', env' = eval_statement x env in
      (match obj' with
       | RETURN_OBJ _ -> obj', env'
       | ERROR _ -> obj', env'
       | _ -> aux xs env' obj')
  in
  aux sl env !null

and eval_expr (e : expression) (env : obj Env.t) : obj =
  match e with
  | IntegerLiteral i -> INT_OBJ i.value
  | BooleanLiteral b -> native_to_bool_obj b.value
  | Prefix e ->
    eval_expr e.right env >>= fun right -> eval_prefix_expr e.operator right
  | Infix e ->
    eval_expr e.left env
    >>= fun left ->
    eval_expr e.right env >>= fun right -> eval_infix_expr e.operator left right
  | If e ->
    eval_expr e.condition env
    >>= fun cond -> eval_if_expr cond e.consequence e.alternative env
  | Identifier i -> eval_identifier i env
  | _ -> !null

and eval_statement (s : statement) (env : obj Env.t) : obj * obj Env.t =
  match s with
  | ExpressionStatement e -> eval_expr e env, env
  | Block sl ->
    let obj', _ = eval_block_statement sl env in
    obj', env
  | Return e ->
    let o = eval_expr e env in
    (match o with
     | ERROR _ -> o, env
     | _ -> RETURN_OBJ o, env)
  | Let e ->
    let o = eval_expr e.value env in
    (match o with
     | ERROR _ -> o, env
     | _ -> !null, Env.add e.name.ident o env)
  | _ -> !null, env

and eval_statements (sl : statement list) (env : obj Env.t) : obj * obj Env.t =
  let rec aux sl env obj =
    match sl with
    | [] -> obj, env
    | x :: xs ->
      let obj', env' = eval_statement x env in
      (match obj' with
       | RETURN_OBJ o -> o, env'
       | ERROR _ -> obj', env'
       | _ -> aux xs env' obj')
  in
  aux sl env !null
;;

let eval (ast : ast) (env : obj Env.t) : obj * obj Env.t =
  eval_statements ast.statements env
;;
