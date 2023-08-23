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
    | Call { callee = c1; arguments = el1 }, 
      Call { callee = c2; arguments = el2 } ->
        if compare_expr c1 c2 then compare_list compare_expr el1 el2
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
  let input = "let x = 5;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_string_literal _ =
  let input = "foobar;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_int_literal _ =
  let input = "5;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_single_prefix_op _ =
  let input = "\n!5;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_all_prefix_op _ =
  let input = "\n!5;\n-15;\n!foobar;\n-foobar;\n!true;\n!false;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_single_infix_op _ =
  let input = "5 + 5;\n" in
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
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
     \t\tfalse == false\n"
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
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_single_expr_op_precedence _ =
  let input = "-a * b\n" in
  let expected =
    { statements =
        [ ExpressionStatement
            (Infix
               { left =
                   Prefix
                     { operator = MINUS; right = Identifier { ident = "a" } }
               ; operator = ASTERISK
               ; right = Identifier { ident = "b" }
               })
        ]
    } in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- SINGLE EXPR OP PRECEDENCE -----\n";
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

let test_multi_expr_op_precedence _ =
  let input =
    "-a * b;\n\
    \  !-a;\n\
    \  a * b + c;\n\
    \  a + b - c;\n\
    \  a * b * c;\n\
    \  a * b / c;\n\
    \  a + b / c;\n\
    \  a + b * c + d / e - f;\n\
    \  5 > 4 == 3 < 4;\n\
    \  5 < 4 != 3 > 4;\n\
    \  3 + 4 * 5 == 3 * 1 + 4 * 5;\n\
    \  true; false;\n\
    \  5 > 3 == false;\n\
    \  3 < 5 == true;\n\
    \  1 + (2 + 3) + 4;\n\
    \  (5 + 5) * 2;\n\
    \  2 / (5 + 5);\n\
    \  (5 + 5) * 2 * (5 + 5);\n\
    \  -(5 + 5);\n\
    \  !(true == true);"
  in
  let expected =
    { statements =
        [ ExpressionStatement
            (Infix
               { left =
                   Prefix
                     { operator = MINUS; right = Identifier { ident = "a" } }
               ; operator = ASTERISK
               ; right = Identifier { ident = "b" }
               })
        ; ExpressionStatement
            (Prefix
               { operator = BANG
               ; right =
                   Prefix
                     { operator = MINUS; right = Identifier { ident = "a" } }
               })
        ; ExpressionStatement
            (Infix
               { left =
                   Infix
                     { left = Identifier { ident = "a" }
                     ; operator = ASTERISK
                     ; right = Identifier { ident = "b" }
                     }
               ; operator = PLUS
               ; right = Identifier { ident = "c" }
               })
        ; ExpressionStatement
            (Infix
               { left =
                   Infix
                     { left = Identifier { ident = "a" }
                     ; operator = PLUS
                     ; right = Identifier { ident = "b" }
                     }
               ; operator = MINUS
               ; right = Identifier { ident = "c" }
               })
        ; ExpressionStatement
            (Infix
               { left =
                   Infix
                     { left = Identifier { ident = "a" }
                     ; operator = ASTERISK
                     ; right = Identifier { ident = "b" }
                     }
               ; operator = ASTERISK
               ; right = Identifier { ident = "c" }
               })
        ; ExpressionStatement
            (Infix
               { left =
                   Infix
                     { left = Identifier { ident = "a" }
                     ; operator = ASTERISK
                     ; right = Identifier { ident = "b" }
                     }
               ; operator = SLASH
               ; right = Identifier { ident = "c" }
               })
        ; ExpressionStatement
            (Infix
               { left = Identifier { ident = "a" }
               ; operator = PLUS
               ; right =
                   Infix
                     { left = Identifier { ident = "b" }
                     ; operator = SLASH
                     ; right = Identifier { ident = "c" }
                     }
               })
        ; ExpressionStatement
            (Infix
               { left =
                   Infix
                     { left =
                         Infix
                           { left = Identifier { ident = "a" }
                           ; operator = PLUS
                           ; right =
                               Infix
                                 { left = Identifier { ident = "b" }
                                 ; operator = ASTERISK
                                 ; right = Identifier { ident = "c" }
                                 }
                           }
                     ; operator = PLUS
                     ; right =
                         Infix
                           { left = Identifier { ident = "d" }
                           ; operator = SLASH
                           ; right = Identifier { ident = "e" }
                           }
                     }
               ; operator = MINUS
               ; right = Identifier { ident = "f" }
               })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix 
                { left = IntegerLiteral { value = 5 }
                ; operator = GT
                ; right = IntegerLiteral { value = 4 }
                })
            ; operator = EQ
            ; right =
              (Infix 
                { left = IntegerLiteral { value = 3 }
                ; operator = LT
                ; right = IntegerLiteral { value = 4 }
                })
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix 
                { left = IntegerLiteral { value = 5 }
                ; operator = LT
                ; right = IntegerLiteral { value = 4 }
                })
            ; operator = NOT_EQ
            ; right =
              (Infix 
                { left = IntegerLiteral { value = 3 }
                ; operator = GT
                ; right = IntegerLiteral { value = 4 }
                })
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix 
                { left = IntegerLiteral { value = 3 }
                ; operator = PLUS
                ; right = 
                  (Infix
                    { left = IntegerLiteral { value = 4 }
                    ; operator = ASTERISK
                    ; right = IntegerLiteral { value = 5 }
                    })
                })
            ; operator = EQ
            ; right =
              (Infix 
                { left = 
                  (Infix
                    { left = IntegerLiteral { value = 3 }
                    ; operator = ASTERISK
                    ; right = IntegerLiteral { value = 1 }
                    })
                ; operator = PLUS
                ; right = 
                  (Infix
                    { left = IntegerLiteral { value = 4 }
                    ; operator = ASTERISK
                    ; right = IntegerLiteral { value = 5 }
                    })
                })
            })
        ; ExpressionStatement (BooleanLiteral { value = true })
        ; ExpressionStatement (BooleanLiteral { value = false })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix 
                { left = IntegerLiteral { value = 5 }
                ; operator = GT
                ; right = IntegerLiteral { value = 3 }
                })
              ; operator = EQ 
            ; right = BooleanLiteral { value = false }
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix 
                { left = IntegerLiteral { value = 3 }
                ; operator = LT
                ; right = IntegerLiteral { value = 5 }
                })
              ; operator = EQ 
            ; right = BooleanLiteral { value = true }
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix
                { left = IntegerLiteral { value = 1 }
                ; operator = PLUS
                ; right = 
                  (Infix
                    { left = IntegerLiteral { value = 2 }
                    ; operator = PLUS
                    ; right = IntegerLiteral { value = 3 }
                    })
                })
              ; operator = PLUS
              ; right = IntegerLiteral { value = 4 }
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix
                { left = IntegerLiteral { value = 5 }
                ; operator = PLUS
                ; right = IntegerLiteral { value = 5 }
                })
            ; operator = ASTERISK
            ; right = IntegerLiteral { value = 2 }
            })
        ; ExpressionStatement
          (Infix
            { left = IntegerLiteral { value = 2 }
            ; operator = SLASH
            ; right =
              (Infix
                { left = IntegerLiteral { value = 5 }
                ; operator = PLUS
                ; right = IntegerLiteral { value = 5 }
                })
            })
        ; ExpressionStatement
          (Infix
            { left = 
              (Infix
                { left = 
                  (Infix
                    { left = IntegerLiteral { value = 5 }
                    ; operator = PLUS
                    ; right = IntegerLiteral { value = 5 }
                    })
                  ; operator = ASTERISK
                  ; right = IntegerLiteral { value = 2 }
                })
            ; operator = ASTERISK
            ; right =
              (Infix
                  { left = IntegerLiteral { value = 5 }
                  ; operator = PLUS
                  ; right = IntegerLiteral { value = 5 }
                  })
            })
        ; ExpressionStatement
            (Prefix 
              { operator = MINUS
              ; right =
                   (Infix
                     { left = IntegerLiteral { value = 5 }
                     ; operator = PLUS
                     ; right = IntegerLiteral { value = 5 }
                     })
               })
        ; ExpressionStatement
            (Prefix 
              { operator = BANG
              ; right =
                   (Infix
                     { left = BooleanLiteral { value = true }
                     ; operator = EQ
                     ; right = BooleanLiteral { value = true }
                     })
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
    Printf.printf "\n----- MULTI OP PRECEDENCE -----\n";
    Printf.printf "\n----- INPUT ----- \n%s" input;
    Printf.printf "\nEXPECTED AST\n";
    pp_ast expected;
    Printf.printf "\nGOT AST\n";
    pp_ast got;
    failwith " are not equal!"
;;

(*
			"add((((a + b) + ((c * d) / f)) + g))",
*)

let test_call_op_precedence _ = 
  let input = "\n\
    \  a + add(b * c) + d;\n\ 
    \  add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8));\n\
    \  add(a + b + c * d / f + g);\n"
  in
  let expected =
    { statements =
        [ ExpressionStatement
          (Infix
            { left = 
              (Infix
                { left = Identifier { ident = "a" }
                ; operator = PLUS
                ; right = 
                  (Call
                    { callee = Identifier { ident = "add" }
                    ; arguments =
                      [
                        (Infix 
                          { left = Identifier { ident = "b" }
                          ; operator = ASTERISK
                          ; right = Identifier { ident = "c" }
                          })
                      ]})
                })
              ; operator = PLUS
              ; right = Identifier { ident = "d" }
            })
        ; ExpressionStatement
          (Call
            { callee = Identifier { ident = "add" }
            ; arguments =
              [ Identifier { ident = "a" }
              ; Identifier { ident = "b" }
              ; IntegerLiteral { value = 1 }
              ; (Infix 
                  { left = IntegerLiteral { value = 2 }
                  ; operator = ASTERISK
                  ; right = IntegerLiteral { value = 3 }
                  })
              ; (Infix 
                  { left = IntegerLiteral { value = 4 }
                  ; operator = PLUS
                  ; right = IntegerLiteral { value = 5 }
                  })
              ; (Call
                { callee = Identifier { ident = "add" }
                ; arguments =
                  [ IntegerLiteral { value = 6 }
                  ; (Infix
                    { left = IntegerLiteral { value = 7 }
                    ; operator = ASTERISK
                    ; right = IntegerLiteral { value = 8 }
                    })
                  ]})
              ]
            })
        ; ExpressionStatement
          (Call
            { callee = Identifier { ident = "add" }
            ; arguments =
              [ (Infix 
                  { left = 
                    (Infix 
                      { left = 
                        (Infix
                          { left = Identifier { ident = "a" }
                          ; operator = PLUS
                          ; right = Identifier { ident = "b" }
                          })
                      ; operator = PLUS
                      ; right =
                        (Infix
                          { left = 
                            (Infix
                              { left = Identifier { ident = "c" }
                              ; operator = ASTERISK
                              ; right = Identifier { ident = "d" }
                              })
                          ; operator = SLASH
                          ; right = Identifier { ident = "f" }
                          })
                      })
                  ; operator = PLUS
                  ; right = Identifier { ident = "g" }
                  })
              ]})
        ]
    }
  in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  match compare_ast expected got with
  | true -> ()
  | false ->
    Printf.printf "\n----- CALL OP PRECEDENCE -----\n";
    Printf.printf "\n----- INPUT ----- \n%s" input;
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
       ; "test_single_expr_op_precedence" >:: test_single_expr_op_precedence
       ; "test_multi_expr_op_precedence" >:: test_multi_expr_op_precedence
       ; "test_call_op_precedence" >:: test_call_op_precedence
       ]
;;

let _ = run_test_tt_main suite
