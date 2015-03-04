open Lex
open Simple_parser

let testcases = [
    ("asdf", Cat
	       (Cat
		  (Cat (Cat (NilNode, Ch 'a', NilNode), NilCh,
			Cat (NilNode, Ch 's', NilNode)),
		   NilCh, Cat (NilNode, Ch 'd', NilNode)),
		NilCh, Cat (NilNode, Ch 'f', NilNode)));
    ("a**|bc", Or (Cat (Cat (NilNode, Ch 'b', NilNode), NilCh, Cat (NilNode, Ch 'c', NilNode)),
		Star (Star (Cat (NilNode, Ch 'a', NilNode)))))
  ]



		  
let run () =
  let aux ind (str, node) =
    let retObj = lex_compile str in
    if snd (theEnd retObj NilNode) = node
    then ()
    else failwith ("case: " ^ string_of_int (ind + 1))
  in
  List.iteri aux testcases;
  ()
;;
let () = run ()

let () = print_string "test succeed!"
