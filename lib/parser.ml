open Token
open Lexer
open Ast

(** TODO: 
  - parse if expressions
  - parse function literals
  - parse block statements
  - parse operator precedence
*)

module Parser = struct
  type parser_error = Unexpected_token of string

  let unexpected_token (expected : string) (got : Token.t) =
    Unexpected_token
      ("expected : " ^ expected ^ "; got : " ^ Token.string_of_token got)
  ;;

  type t =
    { lexer : Lexer.t
    ; cur_token : Token.t
    ; peek_token : Token.t
    ; err_messages : parser_error list (* debug messages *)
    }

  type operator = 
    | LOWEST 
    | EQUALS        (* == *)
    | LESSGREATER   (* > or < *)
    | SUM           (* + *)
    | PRODUCT       (* * *)
    | PREFIX        (* -X or !X *)
    | CALL          (* foo(X) *)
  ;;

  let precedence (op : operator) : int = 
    match op with
    | LOWEST -> 0
    | EQUALS -> 1
    | LESSGREATER -> 2
    | SUM -> 3
    | PRODUCT -> 4
    | PREFIX -> 5
    | CALL -> 6
  ;;

  (*
  	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.SLASH:    PRODUCT,
	token.ASTERISK: PRODUCT,
	token.LPAREN:   CALL,
  *)

  let token_to_precedence (t : Token.t) =
    match t with
    | Token.EQ -> EQUALS
    | Token.NOT_EQ -> EQUALS
    | Token.PLUS -> SUM
    | Token.MINUS -> SUM
    | Token.SLASH -> PRODUCT
    | Token.ASTERISK -> PRODUCT
    | Token.LPAREN -> CALL
    | _ -> LOWEST

  let init (lexer : Lexer.t) : t =
    let lexer, cur_token = Lexer.next_token lexer in
    let lexer, peek_token = Lexer.next_token lexer in
    { lexer; cur_token; peek_token; err_messages = [] }
  ;;

  let next_token (parser : t) : t =
    let cur_token = parser.peek_token in
    let lexer, peek_token = Lexer.next_token parser.lexer in
    { lexer; cur_token; peek_token; err_messages = parser.err_messages }
  ;;

  let push_error (parser : t) (err : parser_error) : t =
    { lexer = parser.lexer
    ; cur_token = parser.cur_token
    ; peek_token = parser.peek_token
    ; err_messages = err :: parser.err_messages
    }
  ;;

  let expect_peek (parser : t) (expected : Token.t) : bool =
    match parser.cur_token, expected with
    | IDENT _, IDENT _ -> true
    | INT _, INT _ -> true
    | cur_token, expected -> cur_token = expected
  ;;

  (* assume current token is integer literal, call next_token *)
  let parse_integer_literal (parser : t) (i : int) : t * Ast.expression =
    next_token parser, IntegerLiteral { value = i }
  ;;

  (* assume current token is boolean literal, call next_token *)
  let parse_boolean_literal (parser : t) (b : bool) : t * Ast.expression =
    next_token parser, BooleanLiteral { value = b }
  ;;

  (* assume current token is string literal, call next_token *)
  let parse_string_literal (parser : t) (s : string) : t * Ast.expression =
    next_token parser, StringLiteral { value = s }
  ;;

  (* check if current token is identifier, call next_token regardless *)
  let parse_identifier (parser : t) : (t * Ast.identifier, t) result =
    match parser.cur_token with
    | IDENT ident -> Ok (next_token parser, { ident })
    | _ ->
      let parser =
        push_error parser (unexpected_token "some ident" parser.cur_token)
      in
      Error (next_token parser)
  ;;


  (*
  let parse_prefix_op (parser : t) : (t * Ast.expression, t) result =
  ;;
  *)
  (* let parse_infix_op (parser : t) (: (t * Ast.expression, t) result = *)

  let parse_expression (parser : t) : (t * Ast.expression, t) result =
    match parser.cur_token with
    | INT i -> Ok (parse_integer_literal parser (int_of_string i))
    | TRUE -> Ok (parse_boolean_literal parser true)
    | FALSE -> Ok (parse_boolean_literal parser false)
    | IDENT ident -> Ok (parse_string_literal parser ident)
    | _ ->
      let parser =
        push_error parser (unexpected_token "some expression" parser.cur_token)
      in
      Error (next_token parser)
  ;;

  let parse_prefix_exp (parser : t) : (t * Ast.expression) result =
    let operator = parser.cur_token in
    Result.bind (parse_expression parser) (fun (parser, left) ->
      Result.bind (parse_expression parser) (fun (parser, right) ->
        Ok (Prefix { operator; left; right })
      )
    )
  ;;

  let parse_let_statement (parser : t) : (t * Ast.statement, t) result =
    Result.bind (parse_identifier parser) (fun (parser, ident) ->
      match parser.cur_token with
      | ASSIGN ->
        let parser = next_token parser in
        Result.bind (parse_expression parser) (fun (parser, expr) ->
          Ok (parser, Ast.Let { name = ident; value = expr }))
      | _ -> let parser =
          push_error
            parser
            (unexpected_token
               (Token.string_of_token Token.ASSIGN)
               parser.cur_token)
        in
        Error (next_token parser))
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

  let parse_statement (parser : t) : (t * Ast.statement, t) result =
    match parser.cur_token with
    | LET -> parse_let_statement (next_token parser)
    | RETURN -> parse_return_statement (next_token parser)
    | _ -> parse_expression_statement parser
  ;;

  let parse (parser : t) : Ast.t =
    let rec aux (parser : t) (ast : Ast.t) : Ast.t =
      match parser.cur_token with
      | EOF -> ast
      | _ -> (match parse_statement parser with
         | Ok (parser, stmt) ->
           aux parser { statements = stmt :: ast.statements }
         | Error parser -> aux parser ast)
    in
    let ast = aux parser { statements = [] } in
    { statements = List.rev ast.statements }
  ;;
end
