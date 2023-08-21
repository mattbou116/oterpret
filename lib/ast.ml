module Ast = struct
  open Token

  type t = { statements : statement list } 
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
    | StringLiteral of { value : string }
    | Identifier of identifier
    | If of
        { condition : expression
        ; consequence : statement
        ; alternative : statement
        }
    | FunctionLiteral of
        { parameters : identifier list
        ; body : expression
        }
    | Prefix of
        { operator : Token.t
        ; left : expression
        ; right : expression
        }
    | Infix of
        { left : expression
        ; operator : Token.t
        ; right : expression
        }
  [@@deriving show];;

  let pp_ast (ast : t) = 
    List.iter (fun x -> Format.printf "%a@\n" pp_statement x) ast.statements
end
