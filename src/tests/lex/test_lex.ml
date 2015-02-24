open Lex

exception LexTestFail of string

let testcases = ref []
			   
let char2str ch = Bytes.make 1 ch
       
let lexVal2str lv : string =
  match lv with
    Id c -> "Id " ^ char2str c
  | Meta c -> "Meta " ^ char2str c
  | _ -> failwith "impossible case"
  

  
       
let run str rstrlist =
  let obj = lex_compile str in
  let rec collect ?(curr=[]) (obj : lexObj) : string list =
    let _doc = "lexVal list -> string list, [Id 'a';..] -> [\"Id a\";..]"
    in
    let _curr = curr 
    and flush_obj = 
      try Some (lex_flush obj) with
	EndOfLexObj -> None
    in
    if flush_obj = None then List.rev _curr
    else
      let (lv, lo) = match flush_obj with
	  Some (a, b) -> (a, b) | _ -> assert false
      in
      collect  ~curr:(lexVal2str lv::_curr) lo
  in
  let cmpList = collect obj in
  let cmpAssocL = List.combine cmpList rstrlist in
  let cmpFunc ind (a, b) =
    if a=b then ()
    else
      raise (LexTestFail ("at :"^string_of_int ind))
  in
  List.iteri cmpFunc cmpAssocL


	     

let test () =
  List.iteri (fun ind (str, rstrlist) ->
	      try run str rstrlist with
		LexTestFail estr ->
		raise (LexTestFail ("case "^string_of_int ind^","^estr))
	    ) !testcases
;;
  testcases := ("qwea",
		["Id q"; "Id w"; "Id e"; "Id a"])
	       :: !testcases
;;
  testcases := ("\\dd? .\\.",
		["Meta d"; "Id d"; "Meta ?"; "Id  "; "Meta ."; "Id ."])
	       :: !testcases
;;
  testcases := ("it('s| is) a test!",
		["Id i";"Id t";"Meta (";"Id '";"Id s";"Meta |";"Id  ";"Id i";
		 "Id s";"Meta )";"Id  ";"Id a";"Id  ";"Id t";"Id e";"Id s";"Id t";
		 "Id !"]
	       )
	       :: !testcases
;;
  testcases := List.rev !testcases
;;
let () = test ()

let () = print_string "lex test successfully!"
