module Repl = struct
  let prompt = ">> "

  let start () =
    let open Lexer in
    let open Token in
    Printf.printf "%s" prompt;
    let user_input = Stdlib.read_line () in
    let rec aux (l : Lexer.lexer) =
      let lexer, token = Lexer.next_token l in
      match token with
      | EOF -> ()
      | _ ->
        Printf.printf "[%s]\n" (Token.string_of_token token);
        aux lexer
    in
    let lexer = Lexer.init user_input in
    aux lexer
  ;;
end
