let () =
  let open Oterpret.Repl in
  let user_name = Unix.getlogin () in
  Printf.printf "Hello %s! This is the Monkey programming language!\n" user_name;
  Printf.printf "Feel free to type in commands\n";
  Repl.start ()
;;
