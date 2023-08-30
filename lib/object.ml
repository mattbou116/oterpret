type obj =
  | INT_OBJ of int
  | BOOL_OBJ of bool
  | RETURN_OBJ of obj
  | FUNCTION_OBJ
  | NULL
  | ERROR of string
[@@deriving show]
;;

let rec string_of_obj (obj : obj) : string =
  match obj with
  | INT_OBJ i -> string_of_int i ^ "\n"
  | BOOL_OBJ b -> string_of_bool b ^ "\n"
  | RETURN_OBJ o -> string_of_obj o
  | FUNCTION_OBJ -> "function\n"
  | NULL -> "null\n"
  | ERROR s -> "Error: " ^ s
;;
