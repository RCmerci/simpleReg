open Simple_parser
open Printf

exception InnerErr of int	(* line number , this err used inside *)
exception MatchErr of string    		
let fstchar (s:string):char= String.get s 0
let tlstr (s:string):string= String.sub s 1 (String.length s - 1)
let string_of_char = String.make 1
(* --------------------------------------------------------- *)
let debug = false
let print_node = ref (fun _a -> ())
		     
let rec _print_node =
  let print_edge e =
    match e with
      Ch c -> print_string ("Ch "^string_of_char c)
    | NilCh -> print_string "NilCh"
  in
  function
    Cat (n1, e, n2) -> (
    print_string "Cat(";
    !print_node n1;
    print_string ", ";
    print_edge e;
    print_string " , ";
    !print_node n2;
    print_string ")"
  )
  | Or (n1, n2) -> (
    print_string "Or (";
    !print_node n1;
    print_string ", ";
    !print_node n2;
    print_string ")";
  )
  | Star n1 -> (
    print_string "Star (";
    !print_node n1;
    print_string ")";
  )
  | NilNode -> print_string "NilNode"

let _ = if debug = false then print_node:= (fun a -> ())
	else print_node := _print_node
(* ------------------------------------------------------- *)
			     
let run str (n:node) =
  let rec nextP n =
    match n with
      Cat (n1, e, n2) -> (e, n2)
    | Or (n1, n2) -> (NilCh, n2)
    | Star n1 -> nextP n1
    | NilNode -> (NilCh, NilNode)
  in
  let rec aux ?(info:edge * node) str n : string =
    match n with
      Or (n1, n2) -> 
      let res_of_n1 = try String.length (aux str n1) with
			InnerErr linen -> 0 in
      if res_of_n1 = 0 then "" 
      else aux str n2
    | Cat (n1, e, n2) -> (
      let () = !print_node n in
      (* let () = print_string "\n" in *)
      let substr1 = aux str n1 in
      match e with
	Ch c -> begin
	  let () = 
	    if (String.length substr1 > 0) && (fstchar substr1 = c)
	    then 
	      print_string (" " ^ (string_of_char c) ^ " ")
	    else begin
		(* printf "wrong : %c and %c\n" c (fstchar substr1) ; *)
		raise (InnerErr __LINE__)
	      end in
	  aux (tlstr substr1) n2 
	end
      | NilCh -> 
	 aux substr1 n2
    )
    | Star n1 -> (
      (* [info] is the next node combined with a `edge` after current `Star` node*)
      let infoP = 
	match info with
	  None -> failwith "[info] should not be [None]"
	| Some a -> a
      in
      match infoP with
	(Nilch, infoN) -> begin
	  let l = try String.length (aux ~info:(nextP infoN) str infoN) with
		    InnerErr _ -> -1 in
	  match l with
	    -1 -> begin
	      let substr1 = aux ~info:infoP str n1  in
	      aux substr1 (Star n1) ~info:infoP
	    end
	  | 0 -> ""
	  | len -> 
	end
    )
       
       (* let substr =  *)
       (* 	 try aux str n1 with *)
       (* 	   InnerErr linen -> str in *)
       (* if str = substr then substr *)
       (* else aux substr n *)
    | NilNode -> str
  in
  let substr =
    try aux str n with 
      InnerErr linen -> " "
  in
  not (String.length substr > 0)












