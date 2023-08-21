module Token = struct
  type t =
    | ILLEGAL
    | EOF
    (* identifiers + literals *)
    | IDENT of string
    | INT of string
    (* operators *)
    | ASSIGN
    | PLUS
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    | LT
    | GT
    | EQ
    | NOT_EQ
    (* delimiters *)
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    (* keywords *)
    | FUNCTION
    | LET
    | TRUE
    | FALSE
    | IF
    | ELSE
    | RETURN
  [@@deriving show];;

  let string_of_token = function
    | ILLEGAL -> "ILLEGAL"
    | EOF -> "EOF"
    (* identifiers + literals *)
    | IDENT s -> "IDENT (" ^ s ^ ")"
    | INT s -> "INT (" ^ s ^ ")"
    (* operators *)
    | ASSIGN -> "="
    | PLUS -> "+"
    | MINUS -> "-"
    | BANG -> "!"
    | ASTERISK -> "*"
    | SLASH -> "/"
    | LT -> "<"
    | GT -> ">"
    | EQ -> "=="
    | NOT_EQ -> "!="
    (* keywords *)
    (* delimiters *)
    | COMMA -> ","
    | SEMICOLON -> ";"
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACE -> "{"
    | RBRACE -> "}"
    (* keywords *)
    | LET -> "let"
    | FUNCTION -> "fn"
    | TRUE -> "true"
    | FALSE -> "false"
    | IF -> "if"
    | ELSE -> "else"
    | RETURN -> "return"
  ;;

  let lookup_ident (ident : string) : t =
    match ident with
    | "fn" -> FUNCTION
    | "let" -> LET
    | "if" -> IF
    | "else" -> ELSE
    | "true" -> TRUE
    | "false" -> FALSE
    | "return" -> RETURN
    | _ -> IDENT ident
  ;;
end
