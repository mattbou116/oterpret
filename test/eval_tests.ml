open OUnit2
open Oterpret

let test_input input =
  let f = fun (x,y) ->
    let lexer = Lexer.init x in
    let parser = Parser.init lexer in
    let ast = Parser.parse parser in
    let o = Eval.eval ast in
    if (y <> o) then 
      let got = Format.asprintf "%a@\n" Object.pp_obj o in
      let exp = Format.asprintf "%a@\n" Object.pp_obj y in
      assert_failure ("Eval failed on: " ^ x ^ "; got: " ^ got ^ "; exp: " ^ exp ^ "\n")
    else ()
  in
  List.iter f input
;;

let test_integer_literal _ =
  let open Object in
  let input =
    [ ("5", INT_OBJ 5)
    ; ("10", INT_OBJ 10)
    ] in
  test_input input
;;

let test_boolean_literal _ =
  let open Object in
  let input =
    [ ("true", BOOL_OBJ true)
    ; ("false", BOOL_OBJ false)
    ] in
  test_input input
;;

let test_int_expression _ =
  let open Object in
  let input =
    [ ("-5", INT_OBJ (-5))
    ; ("-10", INT_OBJ (-10))
    ; ("5 + 5 + 5 + 5 - 10", INT_OBJ 10)
    ; ("2 * 2 * 2 * 2 * 2", INT_OBJ 32)
    ; ("-50 + 100 + -50", INT_OBJ 0)
    ; ("5 * 2 + 10", INT_OBJ 20)
    ; ("5 + 2 * 10", INT_OBJ 25)
    ; ("20 + 2 * -10", INT_OBJ 0)
    ; ("50 / 2 * 2 + 10", INT_OBJ 60)
    ; ("2 * (5 + 10)", INT_OBJ 30)
    ; ("3 * 3 * 3 + 10", INT_OBJ 37)
    ; ("3 * (3 * 3) + 10", INT_OBJ 37)
    ; ("(5 + 10 * 2 + 15 / 3) * 2 + -10", INT_OBJ 50)
    ]
  in
  test_input input
;;

let test_bool_expression _ =
  let open Object in
  let input =
    [ ("1 < 2", BOOL_OBJ true)
    ; ("1 > 2", BOOL_OBJ false)
    ; ("1 < 1", BOOL_OBJ false)
    ; ("1 > 1", BOOL_OBJ false)
    ; ("1 == 1", BOOL_OBJ true)
    ; ("1 != 1", BOOL_OBJ false)
    ; ("1 == 2", BOOL_OBJ false)
    ; ("1 != 2", BOOL_OBJ true)
    ; ("true == true", BOOL_OBJ true)
    ; ("false == false", BOOL_OBJ true)
    ; ("true == false", BOOL_OBJ false)
    ; ("true != false", BOOL_OBJ true)
    ; ("false != true", BOOL_OBJ true)
    ; ("(1 < 2) == true", BOOL_OBJ true)
    ; ("(1 < 2) == false", BOOL_OBJ false)
    ; ("(1 > 2) == true", BOOL_OBJ false)
    ; ("(1 > 2) == false", BOOL_OBJ true)
    ]
  in
  test_input input
;;

let test_bang_operator _ =
  let open Object in
  let input =
    [ ("!true", BOOL_OBJ false)
    ; ("!false", BOOL_OBJ true)
    ; ("!5", BOOL_OBJ false)
    ; ("!!true", BOOL_OBJ true)
    ; ("!!false", BOOL_OBJ false)
    ; ("!!5", BOOL_OBJ true)
    ]
  in
  test_input input
;;

let test_int_if_expression _ =
  let open Object in
  let input = 
    [ ("if (true) { 10 }", INT_OBJ 10)
    ; ("if (1) { 10 }", INT_OBJ 10)
    ; ("if (1 < 2) { 10 }", INT_OBJ 10)
    ; ("if (1 > 2) { 10 } else { 20 }", INT_OBJ 20)
    ; ("if (1 < 2) { 10 } else { 20 }", INT_OBJ 10)
    ]
  in
  test_input input
;;

let test_null_if_expression _ =
  let open Object in
  let input =
    [ ("if (false) { 10 }", NULL)
    ; ("if (1 > 2) { 10 }", NULL)
    ]
  in
  test_input input
;;

let test_single_return_statement _ =
  let open Object in
  let input =
    [ ("return 10;", RETURN_OBJ (INT_OBJ 10))
    ; ("return 10; 9;", RETURN_OBJ (INT_OBJ 10))
    ; ("return 2 * 5; 9;", RETURN_OBJ (INT_OBJ 10))
    ; ("9; return 2 * 5; 9;", RETURN_OBJ (INT_OBJ 10))
    ; ("if (10 > 1) { return 10; }", RETURN_OBJ (INT_OBJ 10))
    ]
  in
  test_input input
;;

let test_let_statement _ =
  let open Object in
  let input =
    [ ("let a = 5; a;", INT_OBJ 5)
    ; ("let a = 5 * 5; a;", INT_OBJ 25)
    ; ("let a = 5; let b = a; b;", INT_OBJ 5)
    ; ("let a = 5; let b = a; let c = a + b + 5; c;", INT_OBJ 15)
    ]
  in
  test_input input
;;

let test_nested_return_statement _ =
  let open Object in
  let one = "
  if (10 > 1) {
    if (10 > 1) {
      return 10;
    }

    return 1;
  }" in

  let two = "
  let f = fn(x) {
    return x;
    x + 10;
  };
  f(10);" in

  let three = "
  let f = fn(x) {
    let result = x + 10;
    return result;
    return 10;
  };
  f(10);" in

  let input =
    [ (one, RETURN_OBJ (INT_OBJ 10))
    ; (two, RETURN_OBJ (INT_OBJ 10))
    ; (three, RETURN_OBJ (INT_OBJ 20))
    ]
  in
  test_input input
;;

let test_error_handling _ =
  let open Object in
  let one = "
  if (10 > 1) {
    if (10 > 1) {
      return true + false;
    }

    return 1;
  }" in
  let input = 
    [ ("5 + true;", ERROR ("unknown operation: (INTEGER + BOOLEAN)"))
    ; ("5 + true; 5;", ERROR ("unknown operation: (INTEGER + BOOLEAN)"))
    ; ("-true", ERROR ("unknown operator: -(BOOLEAN)"))
    ; ("true + false;", ERROR ("unknown operator: (BOOLEAN + BOOLEAN)"))
    ; ("true + false + true + false;", ERROR ("unknown operator: (BOOLEAN + BOOLEAN)"))
    ; ("5; true + false; 5", ERROR ("unknown operator: (BOOLEAN + BOOLEAN)"))
    ; ("if (10 > 1) { true + false; }", ERROR ("unknown operator: (BOOLEAN + BOOLEAN)"))
    ; (one, ERROR ("unknown operator: (BOOLEAN + BOOLEAN)"))
    ;	("foobar", ERROR ("identifier not found: (foobar)"))
    ] 
  in
  test_input input
;;

let suite =
  "eval"
  >::: [ "test_integer_literal" >:: test_integer_literal
       ; "test_boolean_literal" >:: test_boolean_literal
       ; "test_int_expression" >:: test_int_expression
       ; "test_bool_expression" >:: test_bool_expression
       ; "test_bang_operator" >:: test_bang_operator
       ; "test_int_if_expression" >:: test_int_if_expression
       ; "test_null_if_expression" >:: test_null_if_expression
       (* ; "test_single_return_statement" >:: test_single_return_statement
       ; "test_let_statement" >:: test_let_statement
       ; "test_nested_return_statement" >:: test_nested_return_statement *)
       ; "test_error_handling" >:: test_error_handling
       ]
;;

let _ = run_test_tt_main suite
