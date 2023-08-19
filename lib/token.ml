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
  ;;

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

  module KeywordMap = Map.Make(String) 

  let keywords = 
    let open KeywordMap in
    empty 
    |> add "fn" FUNCTION
    |> add "let" LET
    |> add "if" IF
    |> add "true" TRUE
    |> add "false" FALSE
    |> add "if" IF
    |> add "else" ELSE
    |> add "return" RETURN
  ;;

  let lookup_ident (ident : string) : t = 
    let open KeywordMap in
    match (find_opt ident keywords) with
      | None -> IDENT ident
      | Some k -> k
  ;;
end

