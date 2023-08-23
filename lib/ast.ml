open Token

type ast = { statements : statement list } 
[@@deriving show]

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | Block of statement list
  | ExpressionStatement of expression
[@@deriving show]

and identifier = { ident : string }
[@@deriving show]

and expression =
  | IntegerLiteral of { value : int }
  | BooleanLiteral of { value : bool }
  | FunctionLiteral of
      { parameters : identifier list
      ; body : statement
      }
  | Identifier of identifier
  | If of
      { condition : expression
      ; consequence : statement
      ; alternative : statement
      }
  | Prefix of
      { operator : token
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : token
      ; right : expression
      }
  | Call of 
      { callee : expression
      ; arguments : expression list
      }
[@@deriving show];;

let pp_ast (ast : ast) = 
  List.iter (fun x -> Format.printf "%a@\n" pp_statement x) ast.statements
