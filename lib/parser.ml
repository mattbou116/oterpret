open Token
open Lexer
open Ast

(** TODO:
    - parse if expressions
    - parse function literals
    - parse block statements
    - parse operator precedence *)

module Parser = struct
  type t =
    { lexer : Lexer.t
    ; cur_token : Token.t
    ; peek_token : Token.t
    }

  type op_precedence =
    | LOWEST
    | EQUALS (* == *)
    | LESSGREATER (* > or < *)
    | SUM (* + *)
    | PRODUCT (* * *)
    | PREFIX (* -X or !X *)
    | CALL (* foo(X) *)

  let precedence (op : op_precedence) : int =
    match op with
    | LOWEST -> 0
    | EQUALS -> 1
    | LESSGREATER -> 2
    | SUM -> 3
    | PRODUCT -> 4
    | PREFIX -> 5
    | CALL -> 6
  ;;

  let token_to_precedence (t : Token.t) =
    match t with
    | EQ -> EQUALS
    | NOT_EQ -> EQUALS
    | PLUS -> SUM
    | MINUS -> SUM
    | SLASH -> PRODUCT
    | ASTERISK -> PRODUCT
    | LPAREN -> CALL
    | _ -> LOWEST
  ;;

  let init (lexer : Lexer.t) : t =
    let lexer, cur_token = Lexer.next_token lexer in
    let lexer, peek_token = Lexer.next_token lexer in
    { lexer; cur_token; peek_token }
  ;;

  let next_token (parser : t) : t =
    let cur_token = parser.peek_token in
    let lexer, peek_token = Lexer.next_token parser.lexer in
    { lexer; cur_token; peek_token }
  ;;

  let expect_peek (parser : t) (expected : Token.t) : bool =
    match parser.cur_token, expected with
    | IDENT _, IDENT _ -> true
    | INT _, INT _ -> true
    | cur_token, expected -> cur_token = expected
  ;;

  (* assume current token is integer literal, call next_token *)
  let parse_integer_literal (i : int) (parser : t) : t * Ast.expression =
    next_token parser, IntegerLiteral { value = i }
  ;;

  (* assume current token is boolean literal, call next_token *)
  let parse_boolean_literal (b : bool) (parser : t) : t * Ast.expression =
    next_token parser, BooleanLiteral { value = b }
  ;;

  (* assume current token is string literal, call next_token *)
  let parse_string_literal (s : string) (parser : t) : t * Ast.expression =
    next_token parser, StringLiteral { value = s }
  ;;

  (* check if current token is identifier, call next_token regardless *)
  let parse_identifier (parser : t) : (t * Ast.identifier, t) result =
    match parser.cur_token with
    | IDENT ident -> Ok (next_token parser, { ident })
    | _ -> Error (next_token parser)
  ;;

  let prefix_with_token (t : Token.t) : (t -> t * Ast.expression) option =
    match t with
    | INT i -> Some (parse_integer_literal @@ int_of_string i)
    | TRUE -> Some (parse_boolean_literal true)
    | FALSE -> Some (parse_boolean_literal false)
    | IDENT ident -> Some (parse_string_literal ident)
    | _ -> None
  ;;

  let rec parse_expression (parser : t) : (t * Ast.expression, t) result =
    (* parse expression as prefix *)
    (* if next token is semicoln, return expression *)
    (* if next token isn't, parse epxression as infix with prefix being left *)
    (*
    match parse_prefix_exp parser with
    | Ok v -> Ok v
    | Error p ->
      (match parse_infix_exp p with
       | Ok p -> Ok p
       | Error _ -> Error (next_token parser))
      *)

  and parse_prefix_exp (parser : t) : (t * Ast.expression, t) result =
    let operator = parser.cur_token in
    let exprfn = token_to_exprfn @@ (next_token parser).cur_token in
    match exprfn with
    | None -> Error (next_token parser)
    | Some exprfn ->
      let parser, right = exprfn parser in
      Ok (next_token parser, Ast.Prefix { operator; right })

  and parse_infix_exp (parser : t) : (t * Ast.expression, t) result =
    let exprfn = token_to_exprfn @@ (next_token parser).cur_token in
    match exprfn with
    | None -> Error (next_token parser)
    | Some exprfn ->
      let parser, left = exprfn parser in
      let operator = parser.cur_token in
      let exprfn = token_to_exprfn @@ (next_token parser).cur_token in
      (match exprfn with
       | None -> Error (next_token parser)
       | Some exprfn ->
         let parser, right = exprfn parser in
         Ok (next_token parser, Ast.Infix { left; operator; right }))
  ;;

  (*
     Result.bind (parse_expression @@ next_token parser)
    @@ fun (parser, left) ->
    let operator = parser.cur_token in
    Result.bind (parse_expression @@ next_token parser) 
    @@ fun (parser, right) ->
      match parser.cur_token with
      | SEMICOLON -> Ok (next_token parser, Ast.Infix { left; operator; right })
      | _ -> Ok (next_token parser, Ast.Infix { left; operator; right })
  *)

  let parse_let_statement (parser : t) : (t * Ast.statement, t) result =
    Result.bind (parse_identifier parser) (fun (parser, ident) ->
      match parser.cur_token with
      | ASSIGN ->
        Result.bind
          (parse_expression @@ next_token parser)
          (fun (parser, expr) ->
            Ok (parser, Ast.Let { name = ident; value = expr }))
      | _ -> Error (next_token parser))
  ;;

  let parse_return_statement (parser : t) : (t * Ast.statement, t) result =
    match parse_expression parser with
    | Ok (parser, expr) -> Ok (parser, Ast.Return expr)
    | Error parser -> Error parser
  ;;

  let parse_expression_statement (parser : t) : (t * Ast.statement, t) result =
    match parse_expression parser with
    | Ok (parser, expr) -> Ok (parser, Ast.ExpressionStatement expr)
    | Error parser -> Error parser
  ;;

  let rec parse_statement (parser : t) : (t * Ast.statement, t) result =
    match parser.cur_token with
    | LET -> parse_let_statement (next_token parser)
    | RETURN -> parse_return_statement (next_token parser)
    | SEMICOLON -> parse_statement (next_token parser)
    | _ -> parse_expression_statement parser
  ;;

  let parse (parser : t) : Ast.t =
    let rec aux (parser : t) (ast : Ast.t) : Ast.t =
      match parser.cur_token with
      | EOF -> ast
      | _ ->
        (match parse_statement parser with
         | Ok (parser, stmt) ->
           aux parser { statements = stmt :: ast.statements }
         | Error parser -> aux parser ast)
    in
    let ast = aux parser { statements = [] } in
    { statements = List.rev ast.statements }
  ;;
end
