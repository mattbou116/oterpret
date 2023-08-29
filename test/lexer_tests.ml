open OUnit2
open Oterpret.Token
open Oterpret.Lexer

let collect_tokens (l : lexer) : token list =
  let rec aux (l : lexer) (acc : token list) =
    let l, t = next_token l in
    match t with
    | EOF -> acc
    | _ -> aux l (t :: acc)
  in
  List.rev (aux l [])
;;

(*
   let rec print_tokens (tl : token list) =
   match tl with
   | [] -> ()
   | t :: tl ->
   Printf.printf "token:%s\n" (string_of_token t);
   print_tokens tl
   ;;
*)

let compare_tokens (expected : token list) (got : token list) : int =
  let comparison t1 t2 =
    if t1 = t2 then 0 else assert_failure "unequal tokens"
  in
  List.compare comparison expected got
;;

let test_next_token_small _ =
  let input = "=+(){},;" in
  let expected : token list =
    [ ASSIGN; PLUS; LPAREN; RPAREN; LBRACE; RBRACE; COMMA; SEMICOLON; EOF ]
  in
  let lexer = init input in
  let got = collect_tokens lexer in
  (*
     Printf.printf "tokenest 0 result\n";
     List.iter (fun t -> Printf.printf "token:%s\n" (Token.string_of_token t)) got;
  *)
  let _ = compare_tokens expected got in
  ()
;;

let test_next_token_spaces _ =
  let input = "  =+(){},;\n" in
  let expected : token list =
    [ ASSIGN; PLUS; LPAREN; RPAREN; LBRACE; RBRACE; COMMA; SEMICOLON; EOF ]
  in
  let lexer = init input in
  let got = collect_tokens lexer in
  (*
     Printf.printf "tokenest 1 result\n";
     List.iter (fun t -> Printf.printf "token:%s\n" (Token.string_of_token t)) got;
  *)
  let _ = compare_tokens expected got in
  ()
;;

let test_next_token_big _ =
  let input =
    "\n\
    \    let five = 5;\n\
    \    let ten = 10;\n\n\
    \    let add = fn(x, y) {\n\
    \      x + y;\n\
    \    };\n\n\
    \    let result = add(five, ten);\n\
    \    !-/*5;\n\
    \    5 < 10 > 5;\n\n\
    \    if (5 < 10) {\n\
    \      return true;\n\
    \    } else {\n\
    \      return false;\n\
    \    }\n\n\
    \    10 == 10;\n\
    \    10 != 9;\n\
    \  "
  in
  let expected : token list =
    [ LET
    ; IDENT "five"
    ; ASSIGN
    ; INT "5"
    ; SEMICOLON
    ; LET
    ; IDENT "ten"
    ; ASSIGN
    ; INT "10"
    ; SEMICOLON
    ; LET
    ; IDENT "add"
    ; ASSIGN
    ; FUNCTION
    ; LPAREN
    ; IDENT "x"
    ; COMMA
    ; IDENT "y"
    ; RPAREN
    ; LBRACE
    ; IDENT "x"
    ; PLUS
    ; IDENT "y"
    ; SEMICOLON
    ; RBRACE
    ; SEMICOLON
    ; LET
    ; IDENT "result"
    ; ASSIGN
    ; IDENT "add"
    ; LPAREN
    ; IDENT "five"
    ; COMMA
    ; IDENT "ten"
    ; RPAREN
    ; SEMICOLON
    ; BANG
    ; MINUS
    ; SLASH
    ; ASTERISK
    ; INT "5"
    ; SEMICOLON
    ; INT "5"
    ; LT
    ; INT "10"
    ; GT
    ; INT "5"
    ; SEMICOLON
    ; IF
    ; LPAREN
    ; INT "5"
    ; LT
    ; INT "10"
    ; RPAREN
    ; LBRACE
    ; RETURN
    ; TRUE
    ; SEMICOLON
    ; RBRACE
    ; ELSE
    ; LBRACE
    ; RETURN
    ; FALSE
    ; SEMICOLON
    ; RBRACE
    ; INT "10"
    ; EQ
    ; INT "10"
    ; SEMICOLON
    ; INT "10"
    ; NOT_EQ
    ; INT "9"
    ; SEMICOLON
    ; EOF
    ]
  in
  let lexer = init input in
  let got = collect_tokens lexer in
  (*
     Printf.printf "tokenest 3 result\n";
     List.iter (fun t -> Printf.printf "token:%s\n" (Token.string_of_token t)) got;
  *)
  let _ = compare_tokens expected got in
  ()
;;

let suite =
  "lexer"
  >::: [ "test_next_token_small" >:: test_next_token_small
       ; "test_next_token_spaces" >:: test_next_token_spaces
       ; "test_next_token_big" >:: test_next_token_big
       ]
;;

let _ = run_test_tt_main suite
