open Token
open Lexer
open Ast

type parser =
  { lexer : lexer
  ; cur_token : token
  ; peek_token : token
  }

type prec =
  |	LOWEST
  |	EQUALS      (* ==  or != *)
  |	LESSGREATER (* > or < *)
  |	SUM         (*  + or - *)
  |	PRODUCT     (* * or / *)
  |	PREFIX      (* -X or !X *)
  |	CALL        (* myFunction(X) *)
;;

let get_prec = function
  | LOWEST -> 0
  | EQUALS -> 1
  | LESSGREATER -> 2
  | SUM -> 3
  | PRODUCT -> 4
  | PREFIX -> 5
  | CALL -> 6
;;

let prec_of_token = function
  | EQ -> EQUALS
  | NOT_EQ -> EQUALS
  | LT -> LESSGREATER
  | GT -> LESSGREATER
  | PLUS -> SUM
  | MINUS -> SUM
  | SLASH -> PRODUCT
  | ASTERISK -> PRODUCT
  | LPAREN -> CALL
  | _ -> LOWEST
;;

let init (lexer : lexer) : parser =
  let lexer, cur_token = Lexer.next_token lexer in
  let lexer, peek_token = Lexer.next_token lexer in
  { lexer; cur_token; peek_token }
;;

let next_token (parser : parser) : parser =
  let cur_token = parser.peek_token in
  let lexer, peek_token = Lexer.next_token parser.lexer in
  { lexer; cur_token; peek_token }
;;

let cur_token_is (p : parser) (t : token) =
  p.cur_token = t
;;

let peek_token_is (p : parser) (t : token) =
  p.peek_token = t
;;

(* parser -> token -> (parser, parser) result *)
let expect_peek (p : parser) (t : token) =
  if p.peek_token = t then Ok (next_token p) else Error p
;;

let expect_peek_not (p : parser) (t : token) =
  if p.peek_token = t then Error p else Ok (next_token p)
;;

(* parser -> int *)
let cur_prec (p : parser) = get_prec @@ prec_of_token p.cur_token

let rec parse (parser : parser) : ast =
  let rec aux (parser : parser) (ast : ast) : ast =
    match parser.cur_token with
    | EOF -> ast
    | _ ->
      begin match parse_statement parser with
       | Ok (parser, stmt) -> aux (next_token parser) { statements = (stmt :: ast.statements) }
       | Error parser -> aux parser ast
      end
  in
  let ast = aux parser { statements = [] } in
  { statements = List.rev ast.statements }

(* parser -> (parser * statement, parser) result *)
and parse_statement (parser : parser) =
  match parser.cur_token with
  | LET -> parse_let_statement (next_token parser)
  | RETURN -> parse_return_statement (next_token parser)
  | _ -> parse_expression_statement parser

(* parser -> (parser * statement, parser) result *)
and parse_let_statement (p : parser) =
  match p.cur_token with
  | IDENT i -> 
      Result.bind (expect_peek p ASSIGN)
      @@ fun p ->
        Result.bind (parse_expression LOWEST @@ next_token p)
        @@ fun (p, value) -> 
          let name = parse_identifier i in
          if p.peek_token = SEMICOLON then
            Ok ((next_token p), Let { name; value })
          else Ok (p, Let { name; value })
  | _ -> Error (next_token p)

(* parser -> (parser * statement, parser) result *)
and parse_return_statement (p : parser) =
  Result.bind (parse_expression LOWEST p) 
  @@ fun (p, expr) -> Ok (p, Return expr)

(* parser -> (parser * statement, parser) result *)
and parse_expression_statement (p : parser) = 
  (* Printf.printf "ENTERED PARSE EXPRESSION STATEMENT\n"; *)
  Result.bind (parse_expression LOWEST p) 
  @@ fun (p, expr) -> 
    if (p.cur_token = SEMICOLON) then
      Ok (next_token p, ExpressionStatement expr)
    else Ok (p, ExpressionStatement expr)

(* parser -> string -> parser * identifier *)
and parse_identifier (ident : string) : identifier = { ident }

(* parser -> int -> parser * expression *)
and parse_integer_literal (i : int) =
  (* Printf.printf "ENTERED PARSE INTEGER LITERAL\n"; *)
  IntegerLiteral { value = i }

(* parser -> (parser * expression, parser) result *)
and parse_prefix_expression (p : parser) =
  (* Printf.printf "ENTERED PARSE PREFIX EXPRESSION\n"; *)
  Result.bind (parse_expression PREFIX @@ next_token p) 
  @@ fun (p', right) -> Ok (p', Prefix { operator = p.cur_token; right })

(* parser -> (parser * expression, parser) result *)
and parse_infix_expression (p : parser) (left : expression) =
  (* Printf.printf "ENTERED PARSE INFIX EXPRESSION\n"; *)
  Result.bind (parse_expression (prec_of_token p.cur_token) @@ next_token p)
  @@ fun (p', right) -> Ok (p', Infix { left; operator = p.cur_token; right })

(* parser -> bool -> parser * expression *)
and parse_boolean_literal (b : bool) =
  BooleanLiteral { value = b }

(* parser -> (parser * expression, parser) result *)  
and parse_grouped_expression (p : parser) =
  (* Printf.printf "ENTERED PARSE GROUP EXPRESSION\n"; *)
  Result.bind (parse_expression LOWEST @@ next_token p)
  @@ fun (p, expr) -> Result.bind (expect_peek p RPAREN)
    @@ fun p -> Ok (p, expr)

(* parser -> (parser * expression, parser) result *)
and parse_if_expression (p : parser) = 
  (* Printf.printf "ENTERED PARSE If EXPRESSION\n"; *)
  Result.bind (expect_peek p LPAREN)
  @@ fun p ->
    (* Printf.printf "ABOUT TO PARSE CONDITION\n"; *)
    Result.bind (parse_expression LOWEST @@ next_token p)
    @@ fun (p, condition) ->
    (* Printf.printf "PARSED CONDITION\n"; *)
      Result.bind (expect_peek p RPAREN)
      @@ fun p ->
        (* Printf.printf "FOUND RPAREN\n"; *)
        Result.bind (expect_peek p LBRACE)
        @@ fun p ->
          (* Printf.printf "ABOUT TO PARSE CONSEQUENCE\n"; *)
          Result.bind (parse_block_statement p) 
          @@ fun (p, consequence) ->
            if (p.peek_token != ELSE) then
              Ok (p, If { condition; consequence; alternative = NoStatement })
            else
                Result.bind (parse_block_statement @@ next_token @@ next_token p) 
                @@ fun (p, alternative) ->
                  Ok (p, If { condition; consequence; alternative })

(* parser -> (parser * statement, parser) result *)
and parse_block_statement (p : parser) = 
  (* Printf.printf "ENTERED PARSE BLOCK STATEMENT\n"; *)
  let rec aux (p : parser) (acc : statement list) =
    match p.cur_token with
    | LBRACE -> aux (next_token p) acc
    | EOF -> Error p
    | SEMICOLON -> aux (next_token p) acc
    | RBRACE -> if (List.length acc) = 0 then Ok (p, NoStatement::acc) 
    else Ok (p, acc)
    | _ -> 
      Result.bind (parse_statement p)
      @@ fun (p, stmt) -> aux (next_token p) (stmt::acc)
  in
  Result.bind (aux p []) 
  @@ fun (p, stmts) -> Ok (p, Block (List.rev stmts))

(* parser -> (parser * expression, parser) result *)
and parse_function_literal (p : parser) =
  Result.bind (expect_peek p LPAREN)
  @@ fun p ->
    Result.bind (parse_function_args p) 
    @@ fun (p, parameters) -> 
      Result.bind (expect_peek p LBRACE)
      @@ fun p ->
        Result.bind (parse_block_statement p) 
        @@ fun (p, body) -> Ok (p, FunctionLiteral { parameters; body })

(* parser -> (parser * identifier list, parser) result *)
and parse_function_args (p : parser) =
  let rec aux (p : parser) (acc : identifier list) = 
    match p.cur_token with
    | LPAREN -> aux (next_token p) acc
    | RPAREN -> Ok (p, acc)
    | COMMA -> aux (next_token p) acc
    | IDENT i -> aux (next_token p) ((parse_identifier i)::acc)
    | _ -> Error (next_token p)
  in
  Result.bind (aux p []) 
  @@ fun (p, idents) -> Ok (p, List.rev idents)

(* parser -> (parser * expression, parser) result *)
and parse_call_expression (p : parser) (callee : expression) = 
  Result.bind (parse_call_arguments p) 
  @@ fun (p, arguments) -> Ok (p, Call { callee; arguments })

(* parser -> (parser * expression list, parser) result *)
and parse_call_arguments (p : parser) =
  let rec aux (p : parser) (acc : expression list) = 
    match p.cur_token with
    | LPAREN -> aux (next_token p) acc
    | RPAREN -> Ok (p, acc)
    | COMMA -> aux (next_token p) acc
    | _ -> Result.bind (parse_expression LOWEST p)
      @@ fun (p, expr) -> aux (next_token p) (expr::acc)
  in
  Result.bind (aux p []) 
  @@ fun (p, exprs) -> Ok (p, List.rev exprs)

(* parser -> (parser * expression, parser) result *)
and prefix_fn (p : parser) =
  (* Printf.printf "ENTERED PREFIX FN\n"; *)
  match p.cur_token with
  | IDENT i -> Ok (p, Identifier (parse_identifier i))
  | INT i -> Ok (p, parse_integer_literal @@ int_of_string i)
  | BANG -> parse_prefix_expression p
  | MINUS -> parse_prefix_expression p
  | TRUE -> Ok (p, parse_boolean_literal true)
  | FALSE -> Ok (p, parse_boolean_literal false)
  | LPAREN -> parse_grouped_expression p
  | IF -> parse_if_expression p
  | FUNCTION -> parse_function_literal p
  | _ -> Error (next_token p)

(* parser -> (expression -> (parser * expression, parser) result) *)
and infix_fn (p : parser) =
  (* Printf.printf "ENTERED INFIX FN\n"; *)
  match p.cur_token with
  | PLUS -> parse_infix_expression p
  | MINUS -> parse_infix_expression p
  | SLASH -> parse_infix_expression p
  | ASTERISK -> parse_infix_expression p
  | EQ -> parse_infix_expression p
  | NOT_EQ -> parse_infix_expression p
  | LT -> parse_infix_expression p
  | GT -> parse_infix_expression p
  | LPAREN -> parse_call_expression p
  | _ -> fun _ -> Error (next_token p)

(* parser -> prec -> expression ->  *)
and order_by_prec (p : parser) (prec : prec) (left : expression) =
  (* Printf.printf "ENTERED ORDERED BY PREC\n"; *)
  if (p.peek_token != SEMICOLON) && (prec < (prec_of_token p.peek_token)) then
    begin match (infix_fn (next_token p) left) with
    | Ok (p, left) -> order_by_prec p prec left
    | Error p -> Ok (p, left)
    end
  else Ok (p, left)

(* parser -> (parser * expression, parser) result *)
and parse_expression (prec : prec) (p : parser) =
  (* Printf.printf "ENTERED PARSE EXPRESSION\n"; *)
  Result.bind (prefix_fn p)
  @@ fun (p, left) -> order_by_prec p prec left
