let prompt = ">> "

let start () =
  let open Object in
  let open Eval in
  (* let open Ast in *)
  (* let open Token in *)
  let rec aux () =
    output_string stdout prompt;
    let user_input = Stdlib.read_line () in
    let lexer = Lexer.init user_input in
    let parser = Parser.init lexer in
    let ast = Parser.parse parser in
    (* pp_ast ast; *)
    let obj = eval ast in
    let _ = Stdlib.output_string stdout @@ string_of_obj obj ^ "\n" in
    aux ()
  in
  aux ()
;;

let monkey = 
"
#     #                                   
##   ##  ####  #    # #    # ###### #   # 
# # # # #    # ##   # #   #  #       # #  
#  #  # #    # # #  # ####   #####    #   
#     # #    # #  # # #  #   #        #   
#     # #    # #   ## #   #  #        #   
#     #  ####  #    # #    # ######   #   
"
;;
