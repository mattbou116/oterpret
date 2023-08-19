open Token

module type Lexer = sig
  type t
  val init : string -> t
  val read_char : t -> t
end

module Lexer = struct
  type t = {
    input: string; 
    pos: int; (* current position in input *)
    read_pos: int; (* current reading position in input (one after pos) *)
    ch: char; (* current character under examination *)
  }

  let update_lexer lexer c : t = 
    {lexer with
      ch = c;
      pos = lexer.read_pos;
      read_pos = lexer.read_pos + 1;
    }
  ;;

  let read_char lexer : t =
    if lexer.read_pos >= (String.length lexer.input) then
      update_lexer lexer '\000'
    else
      update_lexer lexer @@ String.get lexer.input lexer.read_pos
  ;;

  let peek_char (lexer : t) : char =
    if lexer.read_pos >= (String.length lexer.input) then
      '\000'
    else
      String.get lexer.input lexer.read_pos
  ;;

  let init inp : t = read_char {
    input = inp;
    pos = -1;
    read_pos = 0;
    ch = '\000'
  }
  ;;

  let is_letter = function
    | 'a'..'z' -> true
    | 'A'..'Z' -> true
    | _ -> false
  ;;

  let is_digit = function
    | '0'..'9' -> true
    | _ -> false
  ;;

  let is_whitespace = function
    | ' ' -> true
    | '\t' -> true
    | '\n' -> true
    | '\r' -> true
    | _ -> false
  ;;

  let rec read_identifier (lexer : t) (acc : string) : (t * string) = 
    let lexer = read_char lexer in
    if (is_letter lexer.ch) then
      read_identifier lexer (acc ^ Char.escaped lexer.ch)
    else
      (lexer, acc)
  ;;

  let rec read_number (lexer : t) (acc : string) : (t * string) =
    let lexer = read_char lexer in
    if (is_digit lexer.ch) then
      read_number lexer (acc ^ Char.escaped lexer.ch)
    else
      (lexer, acc)
  ;;

  let rec skip_whitespace (lexer : t) : t =
    if (is_whitespace lexer.ch) then
      let lexer = read_char lexer in
      skip_whitespace lexer
    else
      lexer
  ;;

  let next_token lexer : t * Token.t = 
    let lexer = skip_whitespace lexer in
    match lexer.ch with
      (* operators *)
      | '=' when (peek_char lexer) = '=' ->
          (read_char @@ read_char lexer, EQ)
      | '=' -> (read_char lexer, ASSIGN) 
      | '+' -> (read_char lexer, PLUS)
      | '-' -> (read_char lexer, MINUS)
      | '!' when (peek_char lexer) = '=' -> 
          (read_char @@ read_char lexer, NOT_EQ)
      | '!' -> (read_char lexer, BANG)
      | '*' -> (read_char lexer, ASTERISK)
      | '/' -> (read_char lexer, SLASH)
      | '<' -> (read_char lexer, LT)
      | '>' -> (read_char lexer, GT)
      (* delimiters *)
      | ',' -> (read_char lexer, COMMA)
      | ';' -> (read_char lexer, SEMICOLON)
      | '(' -> (read_char lexer, LPAREN)
      | ')' -> (read_char lexer, RPAREN)
      | '{' -> (read_char lexer, LBRACE)
      | '}' -> (read_char lexer, RBRACE)
      | '\000' -> (read_char lexer, EOF)
      | c when (is_letter c) -> 
          let (lexer, ident) = read_identifier lexer (Char.escaped c) in
          (lexer, Token.lookup_ident ident)
      | d when (is_digit d) -> 
          let (lexer, num) = read_number lexer (Char.escaped d) in
          (lexer, INT num)
      | _ -> (read_char lexer, ILLEGAL)
  ;;
end
