let prompt = ">> "

let start () =
  let open Lexer in
  let open Token in
  let open Ast in
  let rec aux () =
    output_string stdout prompt;
    let user_input = Stdlib.read_line () in
    let lexer = Lexer.init user_input in
    let parser = Parser.init lexer in
    let ast = Parser.parse parser in
    pp_ast ast;
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
