let prompt = ">> "

let start () =
  let open Object in
  let open Eval in
  let open Environment in
  (* let open Ast in *)
  (* let open Token in *)
  let empty_env = Env.empty in
  let rec aux (env : obj Env.t) =
    output_string stdout prompt;
    let user_input = Stdlib.read_line () in
    let lexer = Lexer.init user_input in
    let parser = Parser.init lexer in
    let ast = Parser.parse parser in
    (* pp_ast ast; *)
    let obj, env' = eval ast env in
    let _ = Stdlib.output_string stdout @@ string_of_obj obj ^ "\n" in
    aux env'
  in
  aux empty_env
;;

let monkey =
  "\n\
   #     #                                   \n\
   ##   ##  ####  #    # #    # ###### #   # \n\
   # # # # #    # ##   # #   #  #       # #  \n\
   #  #  # #    # # #  # ####   #####    #   \n\
   #     # #    # #  # # #  #   #        #   \n\
   #     # #    # #   ## #   #  #        #   \n\
   #     #  ####  #    # #    # ######   #   \n"
;;
