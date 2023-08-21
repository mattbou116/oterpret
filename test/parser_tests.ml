open OUnit2
open Oterpret.Lexer
open Oterpret.Ast
open Oterpret.Parser

let rec compare_list (f : 'a -> 'a -> bool) (l1 : 'a list) (l2 : 'a list) =
  match l1, l2 with
  | [], [] -> true
  | x :: xs, y :: ys -> if f x y then compare_list f xs ys else false
  | _, _ -> false
;;

let compare_ast (expected : Ast.t) (got : Ast.t) =
  let rec compare_stmt e g : bool =
    match e, g with
    | Ast.Let { name = n1; value = v1 }, Ast.Let { name = n2; value = v2 } ->
      if compare_ident n1 n2
      then compare_expr v1 v2
      else false
    | Ast.Return e1, Ast.Return e2 -> compare_expr e1 e2
    | Ast.Block sl1, Ast.Block sl2 -> compare_list compare_stmt sl1 sl2
    | Ast.ExpressionStatement e1, Ast.ExpressionStatement e2 ->
      if compare_expr e1 e2
      then true
      else false
    | _ -> false
  and compare_expr e g : bool =
    match e, g with
    | Ast.IntegerLiteral { value = v1 }, Ast.IntegerLiteral { value = v2 } ->
      v1 = v2
    | Ast.BooleanLiteral { value = v1 }, Ast.BooleanLiteral { value = v2 } ->
      v1 = v2
    | Ast.StringLiteral { value = v1 }, Ast.StringLiteral { value = v2 } ->
      v1 = v2
    | ( Ast.If { condition = e1; consequence = c1; alternative = a1 }
      , Ast.If { condition = e2; consequence = c2; alternative = a2 } ) ->
      if compare_expr e1 e2
      then
        if compare_stmt c1 c2
        then compare_stmt a1 a2
        else false
      else false
    | ( Ast.FunctionLiteral { parameters = pl1; body = b1 }
      , Ast.FunctionLiteral { parameters = pl2; body = b2 } ) ->
      if compare_list compare_ident pl1 pl2
      then compare_expr b1 b2
      else false
    | _ -> false
  and compare_ident e g : bool =
    match e, g with
    | { ident = i1 }, { ident = i2 } ->
      i1 = i2
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

let test_single_let_statement _ =
  let input = "let x = 5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : Ast.t =
    { statements =
        [ Ast.Let
            { name = { ident = "x" }; value = IntegerLiteral { value = 5 } }
        ]
    }
  in
  match (compare_ast expected got) with
  | true -> ()
  | false -> Printf.printf "\nSINGLE LET STATEMENT\n";
      Ast.pp_ast got;
      failwith "Single let statement test failed"
;;

let test_multi_let_statement _ =
  let input =
    "\n      let x = 5;\n      let y = true;\n      let foobar = y;\n    "
  in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : Ast.t =
    { statements =
        [ Ast.Let
            { name = { ident = "x" }; value = IntegerLiteral { value = 5 } }
        ; Ast.Let
            { name = { ident = "y" }; value = BooleanLiteral { value = true } }
        ; Ast.Let
            { name = { ident = "foobar" }
            ; value = StringLiteral { value = "y" }
            }
        ]
    }
  in
  match (compare_ast expected got) with
  | true -> ()
  | false -> Printf.printf "\nMULTI LET STATEMENT\n";
      Ast.pp_ast got;
      failwith "Asts are not equal!"
;;

let test_return_statement _ =
  let input = "\n    return 5;\n    return true;\n    return foobar;\n  " in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : Ast.t =
    { statements =
        [ Ast.Return (IntegerLiteral { value = 5 })
        ; Ast.Return (BooleanLiteral { value = true })
        ; Ast.Return (StringLiteral { value = "foobar" })
        ]
    }
  in
  match (compare_ast expected got) with
  | true -> ()
  | false -> Printf.printf "\nRETURN STATEMENT\n";
      Ast.pp_ast got;
      failwith "Asts are not equal!"
;;

let test_string_literal _ =
  let input = "foobar;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : Ast.t =
    { statements =
        [ Ast.ExpressionStatement (StringLiteral { value = "foobar" }) ]
    }
  in
  match (compare_ast expected got) with
  | true -> ()
  | false -> Printf.printf "\nSTRING LITERAL\n";
      Ast.pp_ast got;
      failwith "Asts are not equal!"
;;

let test_int_literal _ =
  let input = "5;" in
  let lexer = Lexer.init input in
  let parser = Parser.init lexer in
  let got = Parser.parse parser in
  let expected : Ast.t =
    { statements = [ Ast.ExpressionStatement (IntegerLiteral { value = 5 }) ] }
  in
  match (compare_ast expected got) with
  | true -> ()
  | false -> Printf.printf "\nINT LITERAL\n";
      Ast.pp_ast got;
      failwith "Asts are not equal!"
;;

let suite =
  "parser"
  >::: [ "test_single_let_statement" >:: test_single_let_statement
       ; "test_multi_let_statement" >:: test_multi_let_statement
       ; "test_return_statement" >:: test_return_statement
       ; "test_string_literal" >:: test_string_literal
       ; "test_int_literal" >:: test_int_literal
       ]
;;

let _ = run_test_tt_main suite
