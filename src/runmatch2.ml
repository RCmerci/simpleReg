open Simple_parser
open Printf

module CharMap = Map.Make (
		     struct
		       type t = edge
		       let compare a b =
			 match (a, b) with
			   (NilCh, NilCh) -> 0
			 | (NilCh, _) -> -1
			 | (_ , NilCh) -> 1
			 | (Ch a, Ch b) -> Char.code a - Char.code b
		     end)
module ChLMap = struct		(* char list map {'a':[something...]} *)
    type 'a t = 'a CharMap.t
    let add ch n m =
      let t = try CharMap.find ch m with Not_found -> []in
      CharMap.add ch (n::t) m
    let empty = CharMap.empty
    let mem = CharMap.mem
    let find = CharMap.find
  end
		  
type acc = Yes | No
type nfaNode = NfaNode of (nfaNode ref list ChLMap.t) * acc | Unused
(* (\* ------------------------------------------------ *\) *)
(* let rec print_indent n = *)
(*   match n with *)
(*     0 -> () *)
(*   | _ -> begin *)
(*       print_string "    "; *)
(*       n-1 |> print_indent end *)
	   
(* let rec print_node ?(indent=0) (n:nfaNode ref) = *)
(*   let print_str s = print_indent indent;print_string s in *)
(*   let NfaNode (a, b) = !n in *)
(*   let () = if b = No then print_str "(No):" else print_str "(Yes):" in *)
(*   CharMap.iter (fun k b -> *)
(* 		let () = *)
(* 		  match k with *)
(* 		    Ch c -> printf "-%c->" c; *)
(* 		  | NilCh -> print_string "-nil->" in *)
(* 		print_newline (); *)
(* 		List.iter (print_node ~indent:(indent+1)) b; *)
(* 		print_newline (); *)
(* 	       ) a; *)
(*   print_newline () *)
(* ------------------------------------------------ *)
let rec string_to_list (str:string) : char list =
  match str with
    "" -> []
  | s -> (String.get s 0) ::
	   string_to_list (String.sub str 1 (String.length str - 1))
  
let run (str: string) (n:node) : bool =
  let nfa:(nfaNode ref * nfaNode ref) ref =
    ref (ref (NfaNode (ChLMap.empty, No)), ref (NfaNode (ChLMap.empty, Yes)))
  in
  let first a = let NfaNode (b, _) = !a in b in
  let firstp a = let NfaNode (b, _) = a in b
  in
  let rec aux nfaNP n =
    let (nfaN1, nfaN2) = nfaNP in
    match n with
      Or (n1, n2) -> begin
	let n11:nfaNode ref = ref (NfaNode (ChLMap.empty, No))
	and n12 = ref (NfaNode (ChLMap.empty, No))
	and n21 = ref (NfaNode (ChLMap.empty, No))
	and n22 = ref (NfaNode (ChLMap.empty, No)) in
	(* let t1 = ChLMap.add NilCh !n11 (first nfaN1) in *)
	(* let t2 = ChLMap.add NilCh !n21 t1 *)
	let t3 = ChLMap.add NilCh nfaN2 (first n12)
	and t4 = ChLMap.add NilCh nfaN2 (first n22) in 
	n12 := NfaNode (t3, No);
	n22 := NfaNode (t4, No);
	aux (n11, n12) n1;
	aux (n21, n22) n2;
	let t1 = ChLMap.add NilCh n11 (first nfaN1) in
	let t2 = ChLMap.add NilCh n21 t1 in
	nfaN1 := NfaNode (t2, No);
      end
    | Cat (n1, e, n2) -> begin
	let n11:nfaNode ref = ref (NfaNode (ChLMap.empty, No))
	and n12 = ref (NfaNode (ChLMap.empty, No)) in
	aux (n12, nfaN2) n2;
	n11 := NfaNode (ChLMap.add e n12 (first n11), No);
	aux (nfaN1, n11) n1;
      end
    | Star n1 -> begin
	let n11:nfaNode ref = ref (NfaNode (ChLMap.empty, No))
	and n12 = ref (NfaNode (ChLMap.empty, No)) in
	nfaN1 := NfaNode (ChLMap.add NilCh nfaN2 (first nfaN1), No);
	aux (n11, n12) n1;
	nfaN1 := NfaNode (ChLMap.add NilCh n11 (first nfaN1), No);
	n12 := NfaNode (ChLMap.add NilCh n11 (first n12), No);
	n12 := NfaNode (ChLMap.add NilCh nfaN2 (first n12), No);
      end
    | NilNode -> nfaN1 := NfaNode (ChLMap.add NilCh nfaN2 (first nfaN1), No)
  in
  let chlist = string_to_list str  in
  let rec auxf (statel:(nfaNode ref as 'a) list):'a list =		(* consume all NilCh *)
    match statel with
      [] -> []
    | hd :: (tl:'a list) ->
       let t:'a list = try ChLMap.find NilCh (first hd) with Not_found -> [] in
       if t = [] then hd :: auxf tl 
       else t @ tl |> auxf
  in
  let f (statel:nfaNode ref list) ch =
    let edgech = Ch ch in
    let tl = List.filter (fun a -> ChLMap.mem edgech (first a)) statel in
    let tl2:nfaNode ref list list = List.map (fun a -> ChLMap.find edgech (first a)) tl in
    let tl3 = List.flatten tl2 in
    auxf tl3
  in
  aux !nfa n;
  
  let secondp a = let NfaNode (_, b) = a in b in
  let second  a = let NfaNode (_, b) = !a in b in
  let initList = auxf [(fst !nfa)] in (* nfaNode ref list *)
  List.fold_left f initList chlist (* the result is the last nfaNodes list *)
  |> List.exists (fun a -> second a = Yes)


