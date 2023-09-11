open Ast

type obj =
  | NULL
  | ERROR of string
  | INT_OBJ of int
  | BOOL_OBJ of bool
  | RETURN_OBJ of obj
  | FUNCTION_OBJ of identifier list * statement list * environment
[@@deriving show]

and environment = string * obj list
[@@ deriving show]
;;

let string_of_env (env : environment) =
  Format.asprintf "%a@\n%!" pp_environment env
;;

let rec string_of_obj (obj : obj) : string =
  match obj with
  | INT_OBJ i -> string_of_int i
  | BOOL_OBJ b -> string_of_bool b
  | RETURN_OBJ o -> string_of_obj o
  | FUNCTION_OBJ (il, sl, e) -> 
      string_of_idents il ^ string_of_ast { statements=sl } ^ string_of_env e
  | NULL -> "null"
  | ERROR msg -> "Error: " ^ msg
;;

let string_of_obj_type (obj : obj) : string = 
  match obj with
  | INT_OBJ _ -> "INTEGER"
  | BOOL_OBJ _ -> "BOOLEAN"
  | RETURN_OBJ _ -> "RETURN_VALUE"
  | FUNCTION_OBJ _ -> "FUNCTION"
  | NULL -> "NULL"
  | ERROR _ -> "ERROR"
;;

let bind (obj : obj) (f : obj -> obj) : obj =
  match obj with
  | ERROR _ -> obj
  | _ -> f obj
;;
