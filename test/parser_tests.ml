open OUnit2
open Oterpret

let rec compare_list (f : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list) =
  match l1, l2 with
  | [], [] -> true
  | x :: xs, y :: ys -> if f x y then compare_list f xs ys else false
  | _, _ -> false
;;

let compare_ast (expected : Ast.ast) (got : Ast.ast) =
  let open Ast in
  let rec compare_stmt e g : bool =
    match e, g with
    | Let { name = n1; value = v1 }, Let { name = n2; value = v2 } ->
      if compare_ident n1 n2 then compare_expr v1 v2 else false
    | Return e1, Return e2 -> compare_expr e1 e2
    | Block sl1, Block sl2 -> compare_list compare_stmt sl1 sl2
    | ExpressionStatement e1, ExpressionStatement e2 ->
      if compare_expr e1 e2 then true else false
    | _ -> false
  and compare_expr e g : bool =
    match e, g with
    | IntegerLiteral { value = v1 }, IntegerLiteral { value = v2 } -> v1 = v2
    | BooleanLiteral { value = v1 }, BooleanLiteral { value = v2 } -> v1 = v2
    | Identifier { ident = i1 }, Identifier { ident = i2 } -> i1 = i2
    | ( If { condition = e1; consequence = c1; alternative = a1 }
      , If { condition = e2; consequence = c2; alternative = a2 } ) ->
      if compare_expr e1 e2
      then if compare_stmt c1 c2 then compare_stmt a1 a2 else false
      else false
    | ( FunctionLiteral { parameters = pl1; body = b1 }
      , FunctionLiteral { parameters = pl2; body = b2 } ) ->
      if compare_list compare_ident pl1 pl2 then compare_stmt b1 b2 else false
    | Prefix { operator = o1; right = r1 }, Prefix { operator = o2; right = r2 }
      -> if o1 = o2 then compare_expr r1 r2 else false
    | ( Infix { left = l1; operator = o1; right = r1 }
      , Infix { left = l2; operator = o2; right = r2 } ) ->
      if o1 = o2
      then if compare_expr l1 l2 then compare_expr r1 r2 else false
      else false
    | _ -> false
  and compare_ident e g : bool =
    match e, g with
    | { ident = i1 }, { ident = i2 } -> i1 = i2
  in
  compare_list compare_stmt expected.statements got.statements
;;

(*
   let rec print_error_messages (el : Parser.parser_error list) =
   match el with
   | [] -> ()
   | e::es -> begin match e with
   | Parser.Unexpected_token msg -> Printf.printf "Unexpected token %s" msg;
   print_error_messages es
   end
   ;;
*)

open Ast

let test_single_let_statement _ =
  let input = "let x = 5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ Let { name = { ident = "x" }; value = IntegerLiteral { value = 5 } } ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- SINGLE LET -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith "Asts are not equal!"
;;

let test_multi_let_statement _ =
  let input =
    "\n      let x = 5;\n      let y = true;\n      let foobar = y;\n    "
  in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ Let { name = { ident = "x" }; value = IntegerLiteral { value = 5 } }
        ; Let
            { name = { ident = "y" }; value = BooleanLiteral { value = true } }
        ; Let
            { name = { ident = "foobar" }; value = Identifier { ident = "y" } }
        ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- MULTI LET -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith "Asts are not equal!"
;;

let test_return_statement _ =
  let input = "\n    return 5;\n    return true;\n    return foobar;\n  " in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ Return (IntegerLiteral { value = 5 })
        ; Return (BooleanLiteral { value = true })
        ; Return (Identifier { ident = "foobar" })
        ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- RETURN STATEMENT -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_string_literal _ =
  let input = "foobar;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements = [ ExpressionStatement (Identifier { ident = "foobar" }) ] }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- STRING LITERAL -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_int_literal _ =
  let input = "5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements = [ ExpressionStatement (IntegerLiteral { value = 5 }) ] }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- INT LITERAL -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_single_prefix_op _ =
  let input = "\n!5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ ExpressionStatement
            (Prefix
               { operator = Token.BANG; right = IntegerLiteral { value = 5 } })
        ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- SINGLE PREFIX -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_all_prefix_op _ =
  let input = "\n!5;\n-15;\n!foobar;\n-foobar;\n!true;\n!false;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ ExpressionStatement
            (Prefix
               { operator = Token.BANG; right = IntegerLiteral { value = 5 } })
        ; ExpressionStatement
            (Prefix
               { operator = Token.MINUS; right = IntegerLiteral { value = 15 } })
        ; ExpressionStatement
            (Prefix
               { operator = Token.BANG
               ; right = Identifier { ident = "foobar" }
               })
        ; ExpressionStatement
            (Prefix
               { operator = Token.MINUS
               ; right = Identifier { ident = "foobar" }
               })
        ; ExpressionStatement
            (Prefix
               { operator = Token.BANG
               ; right = BooleanLiteral { value = true }
               })
        ; ExpressionStatement
            (Prefix
               { operator = Token.BANG
               ; right = BooleanLiteral { value = false }
               })
        ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- ALL PREFIX -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_single_infix_op _ =
  let input = "5 + 5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : ast =
    { statements =
        [ ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.PLUS
               ; right = IntegerLiteral { value = 5 }
               })
        ]
    }
  in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- SINGLE INFIX -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_all_infix_op _ =
  let input =
    "5 + 5;\n\
     \t\t5 - 5;\n\
     \t\t5 * 5;\n\
     \t\t5 / 5;\n\
     \t\t5 > 5;\n\
     \t\t5 < 5;\n\
     \t\t5 == 5;\n\
     \t\t5 != 5;\n\
     \t\tfoobar + barfoo;\n\
     \t\tfoobar - barfoo;\n\
     \t\tfoobar * barfoo;\n\
     \t\tfoobar / barfoo;\n\
     \t\tfoobar > barfoo;\n\
     \t\tfoobar < barfoo;\n\
     \t\tfoobar == barfoo;\n\
     \t\tfoobar != barfoo;\n\
     \t\ttrue == true\n\
     \t\ttrue != false\n\
     \t\tfalse == false"
  in
  let expected : ast =
    { statements =
        [ ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.PLUS
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.MINUS
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.ASTERISK
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.SLASH
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.GT
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.LT
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.EQ
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = IntegerLiteral { value = 5 }
               ; operator = Token.NOT_EQ
               ; right = IntegerLiteral { value = 5 }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.PLUS
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.MINUS
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.ASTERISK
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.SLASH
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.GT
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.LT
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.EQ
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "foobar" }
               ; operator = Token.NOT_EQ
               ; right = Identifier { ident = "barfoo" }
               })
        ; ExpressionStatement
            (Infix
               { left = BooleanLiteral { value = true }
               ; operator = Token.EQ
               ; right = BooleanLiteral { value = true }
               })
        ; ExpressionStatement
            (Infix
               { left = BooleanLiteral { value = true }
               ; operator = Token.NOT_EQ
               ; right = BooleanLiteral { value = false }
               })
        ; ExpressionStatement
            (Infix
               { left = BooleanLiteral { value = false }
               ; operator = Token.EQ
               ; right = BooleanLiteral { value = false }
               })
        ]
    }
  in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- ALL INFIX -----\n";
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let suite =
  "parser"
  >::: [ "test_single_let_statement" >:: test_single_let_statement
       ; "test_multi_let_statement" >:: test_multi_let_statement
       ; "test_return_statement" >:: test_return_statement
       ; "test_string_literal" >:: test_string_literal
       ; "test_int_literal" >:: test_int_literal
       ; "test_single_prefix_op" >:: test_single_prefix_op
       ; "test_all_prefix_op" >:: test_all_prefix_op
       ; "test_single_infix_op" >:: test_single_infix_op
       ; "test_all_infix_op" >:: test_all_infix_op
       ]
;;

let _ = run_test_tt_main suite
