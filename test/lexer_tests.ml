open OUnit2
open Oterpret.Token
open Oterpret.Lexer

(* 
let rec print_tokens (l : Lexer.t) (tl : Token.t list) =
  let (l, t') = Lexer.next_token l in
  match tl with
    | [] -> ()
    | t :: tl ->
        Printf.printf "expected:%s got:%s\n" 
        (string_of_token t)
        (string_of_token t');
        print_tokens l tl
;;

let lexer = Lexer.init input in
print_tokens lexer expected
*)

let rec compare_tokens (l : Lexer.t) (tl : Token.t list) =
  let (l, t') = Lexer.next_token l in
  match tl with
    | [] -> ()
    | t::tl -> assert_equal t t'; compare_tokens l tl
;;

let test_next_token_small _ =
  let input = "=+(){},;" in
  let expected : Token.t list = [
    ASSIGN;
    PLUS;
    LPAREN;
    RPAREN;
    LBRACE;
    RBRACE;
    COMMA;
    SEMICOLON;
    EOF
  ] in

  let lexer = Lexer.init input in
  compare_tokens lexer expected
;;

let test_next_token_spaces _ =
  let input = "  =+(){},;\n" in
  let expected : Token.t list = [
    ASSIGN;
    PLUS;
    LPAREN;
    RPAREN;
    LBRACE;
    RBRACE;
    COMMA;
    SEMICOLON;
    EOF
  ] in

  let lexer = Lexer.init input in
  compare_tokens lexer expected
;;

let test_next_token_big _ =
  let input = "
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
      x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
      return true;
    } else {
      return false;
    }

    10 == 10;
    10 != 9;
  " in

  let expected : Token.t list = [
    LET;
    IDENT "five";
    ASSIGN;
    INT "5";
    SEMICOLON;
    LET;
    IDENT "ten";
    ASSIGN;
    INT "10";
    SEMICOLON;
    LET;
    IDENT "add";
    ASSIGN;
    FUNCTION;
    LPAREN;
    IDENT "x";
    COMMA;
    IDENT "y";
    RPAREN;
    LBRACE;
    IDENT "x";
    PLUS;
    IDENT "y";
    SEMICOLON;
    RBRACE;
    SEMICOLON;
    LET;
    IDENT "result";
    ASSIGN;
    IDENT "add";
    LPAREN;
    IDENT "five";
    COMMA;
    IDENT "ten";
    RPAREN;
    SEMICOLON;
    BANG;
    MINUS;
    SLASH;
    ASTERISK;
    INT "5";
    SEMICOLON;
    INT "5";
    LT;
    INT "10";
    GT;
    INT "5";
    SEMICOLON;
    IF;
    LPAREN;
    INT "5";
    LT;
    INT "10";
    RPAREN;
    LBRACE;
    RETURN;
    TRUE;
    SEMICOLON;
    RBRACE;
    ELSE;
    LBRACE;
    RETURN;
    FALSE;
    SEMICOLON;
    RBRACE;
    INT "10";
    EQ;
    INT "10";
    SEMICOLON;
    INT "10";
    NOT_EQ;
    INT "9";
    SEMICOLON;
    EOF;
  ] in

  let lexer = Lexer.init input in
  compare_tokens lexer expected
;;

let suite =
  "lexer" >::: [
    "test_next_token_small" >:: test_next_token_small;
    "test_next_token_spaces" >:: test_next_token_spaces;
    "test_next_token_big" >:: test_next_token_big;
  ]

let _ = run_test_tt_main suite

