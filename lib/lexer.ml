
type lexer =
  { input : string
  ; pos : int (* current position in input *)
  ; read_pos : int (* current reading position in input (one after pos) *)
  ; ch : char (* current character under examination *)
  }

let update_l l c : lexer =
  { l with ch = c; pos = l.read_pos; read_pos = l.read_pos + 1 }
;;

let read_char l : lexer =
  if l.read_pos >= String.length l.input
  then update_l l '\000'
  else update_l l @@ String.get l.input l.read_pos
;;

let peek_char (l : lexer) : char =
  if l.read_pos >= String.length l.input
  then '\000'
  else String.get l.input l.read_pos
;;

let init inp : lexer =
  read_char { input = inp; pos = -1; read_pos = 0; ch = '\000' }
;;

let is_letter = function
  | 'a' .. 'z' -> true
  | 'A' .. 'Z' -> true
  | _ -> false
;;

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_whitespace = function
  | ' ' -> true
  | '\t' -> true
  | '\n' -> true
  | '\r' -> true
  | _ -> false
;;

let rec read_identifier (l : lexer) (acc : string) : lexer * string =
  let l = read_char l in
  if is_letter l.ch
  then read_identifier l (acc ^ Char.escaped l.ch)
  else l, acc
;;

let rec read_number (l : lexer) (acc : string) : lexer * string =
  let l = read_char l in
  if is_digit l.ch
  then read_number l (acc ^ Char.escaped l.ch)
  else l, acc
;;

let rec skip_whitespace (l : lexer) : lexer =
  if is_whitespace l.ch
  then (
    let l = read_char l in
    skip_whitespace l)
  else l
;;

open Token
let next_token l : lexer * token =
  let l = skip_whitespace l in
  match l.ch with
  (* operators *)
  | '=' when peek_char l = '=' -> read_char @@ read_char l, EQ
  | '=' -> read_char l, ASSIGN
  | '+' -> read_char l, PLUS
  | '-' -> read_char l, MINUS
  | '!' when peek_char l = '=' -> read_char @@ read_char l, NOT_EQ
  | '!' -> read_char l, BANG
  | '*' -> read_char l, ASTERISK
  | '/' -> read_char l, SLASH
  | '<' -> read_char l, LT
  | '>' -> read_char l, GT
  (* delimiters *)
  | ',' -> read_char l, COMMA
  | ';' -> read_char l, SEMICOLON
  | '(' -> read_char l, LPAREN
  | ')' -> read_char l, RPAREN
  | '{' -> read_char l, LBRACE
  | '}' -> read_char l, RBRACE
  | '\000' -> read_char l, EOF
  | c when is_letter c ->
    let l, ident = read_identifier l (Char.escaped c) in
    l, Token.lookup_ident ident
  | d when is_digit d ->
    let l, num = read_number l (Char.escaped d) in
    l, INT num
  | _ -> read_char l, ILLEGAL
;;

