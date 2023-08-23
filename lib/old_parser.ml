(*
open Token
open Lexer
open Ast

(** TODO:
    - parse if expressions
    - parse function literals
    - parse block statements
    - parse operator precedence *)

type parser =
  { lexer : lexer
  ; cur_token : token
  ; peek_token : token
  }

type precedence =
  |	LOWEST
  |	EQUALS      (* ==  or != *)
  |	LESSGREATER (* > or < *)
  |	SUM         (*  + or - *)
  |	PRODUCT     (* * or / *)
  |	PREFIX      (* -X or !X *)
  |	CALL        (* myFunction(X) *)
;;

let get_precedence = function
  | LOWEST -> 0
  | EQUALS -> 1
  | LESSGREATER -> 2
  | SUM -> 3
  | PRODUCT -> 4
  | PREFIX -> 5
  | CALL -> 6
;;

let token_to_precedence = function
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

(* parser -> token -> (parser, parser) result *)
let expect_cur (p : parser) (t : token) =
  (* Printf.printf "ENTERED EXPECT CUR\n"; *)
  if p.cur_token = t then Ok p else Error (next_token p)
;;

(* parser -> int *)
let cur_precedence (p : parser) =
  get_precedence @@ token_to_precedence p.cur_token
;;

(* parser -> int -> parser * expression *)
let parse_integer_literal (p : parser) (i : int) =
  (* Printf.printf "ENTERED PARSE INTEGER LITERAL\n"; *)
  next_token p, IntegerLiteral { value = i }
;;

(* parser -> bool -> parser * expression *)
let parse_boolean_literal (p : parser) (b : bool) =
  (* Printf.printf "ENTERED PARSE BOOLEAN LITERAL\n"; *)
  next_token p, BooleanLiteral { value = b }
;;

(* parser -> string -> parser * identifier *)
let parse_identifier (p : parser) (ident : string) =
  (* Printf.printf "ENTERED PARSE IDENTIFIER\n"; *)
  next_token p, { ident }
;;

(* parser -> (parser * token, parser) result *)
let check_prefix_operator (p : parser) =
  (* Printf.printf "ENTERED PARSE PREFIX OP\n"; *)
  match p.cur_token with
  | BANG -> Ok (next_token p, BANG)
  | MINUS -> Ok (next_token p, MINUS)
  | LPAREN -> Ok (next_token p, LPAREN)
  | _ -> Error (next_token p)
;;

(* parser -> (parser * token, parser) result *)
let check_infix_operator (p : parser) =
 (*  Printf.printf "ENTERED PARSE INFIX OP\n"; *)
  match p.cur_token with
  | PLUS -> Ok (next_token p, PLUS)
  | MINUS -> Ok (next_token p, MINUS)
  | ASTERISK -> Ok (next_token p, ASTERISK)
  | SLASH -> Ok (next_token p, SLASH)
  | EQ -> Ok (next_token p, EQ)
  | NOT_EQ -> Ok (next_token p, NOT_EQ)
  | LT -> Ok (next_token p, LT)
  | GT -> Ok (next_token p, GT)
  | LPAREN -> Ok (next_token p, LPAREN)
  | _ -> Error (next_token p)
;;

(* parser -> (parser * expression, parser) result *)
let rec parse_infix_literals (p : parser) =
  match p.cur_token with
  | IDENT i -> let (p, ident) = parse_identifier p i in
    Ok (p, Identifier ident)
  | INT i -> Ok (parse_integer_literal p @@ int_of_string i)
  | TRUE -> Ok (parse_boolean_literal p true) 
  | FALSE -> Ok (parse_boolean_literal p false) 
  | FUNCTION -> parse_function_literal p
  | IF -> parse_if_expr p
  | LPAREN -> parse_call_expression p
  | _ -> Error (next_token p)

(* parser -> token -> (parser * expression, parser) result *)
and parse_prefix (p : parser) (operator : token) =
  (* Printf.printf "ENTERED PARSE PREFIX\n";*)
  Result.bind (parse_expression PREFIX p) 
  @@ fun (p, right) -> Ok (p, Prefix { operator; right })

(* parser -> expression -> (parser * expression, parser) result *)
and parse_infix (p : parser) (left : expression) =
  (* Printf.printf "ENTERED PARSE INFIX\n"; *)
  Result.bind (check_infix_operator p) 
  @@ fun (p, operator) ->
    Result.bind (parse_expression (token_to_precedence operator) p)
    @@ fun (p, right) -> Ok (p, Infix { left; operator; right})

(* MAKE NOTE OF SEMICOLN AFTER PREFIX/INFIX *)
(* WHERE ARE WE PARSING LITERALS *)
and structure_precedence (p : parser) (pred : precedence) (left : expression) =
  if (get_precedence pred) < (cur_precedence p) then
    Result.bind (parse_infix p left) 
    @@ fun (p, left) -> structure_precedence p pred left
  else Ok (p, left)

(* parser -> (parser * expression, parser) result *)
and parse_expression (pred : precedence) (p : parser) =
  (* Printf.printf "ENTERED PARSE EXPRESSION\n"; *)
  begin match (check_prefix_operator p) with
  | Ok (p, operator) ->
      Result.bind (parse_prefix p operator) 
      @@ fun (p, left) -> 
        structure_precedence p pred left
  | Error _ -> 
    Result.bind (parse_infix_literals p)
    @@ fun (p, left) ->
      structure_precedence p pred left
  end

(* parser -> (parser * statement, parser) result *)
and parse_let_statement (p : parser) =
  (* Printf.printf "ENTERED PARSE LET STATEMENT\n"; *)
  match p.cur_token with
  | IDENT i -> let p, ident = parse_identifier p i in
    Result.bind (expect_cur p ASSIGN)
    @@ fun p -> 
      Result.bind (parse_expression LOWEST @@ next_token p)
      @@ fun (p, expr) -> Ok (p, Let { name = ident; value = expr })
  | _ -> Error (next_token p)

(* parser -> (parser * statement, parser) result *)
and parse_return_statement (p : parser) =
  (* Printf.printf "ENTERED PARSE RETURN STATEMENT\n"; *)
  Result.bind (parse_expression LOWEST p) 
  @@ fun (p, expr) -> Ok (p, Return expr)

(* parser -> (parser * statement, parser) result *)
and parse_expression_statement (p : parser) = 
  (* Printf.printf "ENTERED PARSE EXPRESSION STATEMENT\n"; *)
  Result.bind (parse_expression LOWEST p) 
  @@ fun (p, expr) -> Ok (p, ExpressionStatement expr)

(* parser -> (parser * statement, parser) result *)
and parse_statement (parser : parser) =
  (* Printf.printf "ENTERED PARSE STATEMENT\n"; *)
  match parser.cur_token with
  | LET -> parse_let_statement (next_token parser)
  | RETURN -> parse_return_statement (next_token parser)
  | SEMICOLON -> parse_statement (next_token parser)
  | _ -> parse_expression_statement parser

(* parser -> (parser * statement, parser) result *)
and parse_block_statement (p : parser) = 
  (* Printf.printf "ENTERED PARSE BLOCK STATEMENT\n"; *)
  let rec aux (p : parser) (acc : statement list) =
    match p.cur_token with
    | LBRACE -> aux (next_token p) acc
    | RBRACE -> Ok (p, acc)
    | EOF -> Error p
    | _ -> Result.bind (parse_statement p)
      @@ fun (p, stmt) -> aux p (stmt::acc)
  in
  Result.bind (aux p []) 
  @@ fun (p, stmts) -> Ok (p, Block (List.rev stmts))

(* parser -> (parser * identifier list, parser) result *)
and parse_arguments (p : parser) (a : arg) =
  (* Printf.printf "ENTERED PARSE FUNCTION ARGS\n"; *)
  let rec aux (p : parser) (acc : identifier list) = 
    match p.cur_token with
    | LPAREN -> aux (next_token p) acc
    | RPAREN -> Ok (p, acc)
    | COMMA -> aux (next_token p) acc
    | IDENT i when a = IDENTIFIER -> 
        let (p, ident) = parse_identifier p i in
        aux p (ident::acc)
    | _ when a = EXPRESSION -> 
        Result.bind (parse_expression LOWEST p)
        @@ fun (p, expr) -> aux p (expr::acc)
    | _ -> Error (next_token p)
  in
  Result.bind (aux p []) 
  @@ fun (p, idents) -> Ok (p, List.rev idents)

(* parser -> (parser * expression, parser) result *)
and parse_function_literal (p : parser) =
  (* Printf.printf "ENTERED PARSE FUNCTION LITERAL\n"; *)
  Result.bind (parse_function_args p) 
  @@ fun (p, parameters) -> 
    Result.bind (parse_block_statement p) 
    @@ fun (p, body) -> Ok (p, FunctionLiteral { parameters; body })

(* parser -> (parser * expression, parser) result *)  
and parse_grouped_expression (p : parser) =
  (* Printf.printf "ENTERED PARSE GROUP EXPRESSION\n"; *)
  Result.bind (parse_expression CALL p)
  @@ fun (p, expr) -> Result.bind (expect_cur p RPAREN)
    @@ fun (p) -> Ok (next_token p, expr)

(* parser -> (parser * expression, parser) result *)
and parse_if_expr (p : parser) = 
  (* Printf.printf "ENTERED PARSE IF\n"; *)
  Result.bind (parse_grouped_expression p)
  @@ fun (p, condition) ->
    Result.bind (parse_statement p) 
    @@ fun (p, consequence) ->
      Result.bind (parse_statement p) 
      @@ fun (p, alternative) ->
        Ok (p, If { condition; consequence; alternative })

(* parser -> (parser * expression, parser) result *)
and parse_call_expression (p : parser) (callee : expression) = 
  Result.bind (parse_call_arguments p) 
  @@ fun (p, arguments) -> Ok (p, Call { callee; arguments })

;;

let parse (parser : parser) : ast =
  (* Printf.printf "ENTERED PARSE\n"; *)
  let rec aux (parser : parser) (ast : Ast.ast) : Ast.ast =
    match parser.cur_token with
    | EOF -> ast
    | _ ->
      (match parse_statement parser with
       | Ok (parser, stmt) -> aux parser { statements = stmt :: ast.statements }
       | Error parser -> aux parser ast)
  in
  let ast = aux parser { statements = [] } in
  { statements = List.rev ast.statements }
;;
*)
