(*
"||" means "or" here.
Rep: the whole expression
Cat&star: concatenation of string or repeat string
Star: repeat string
Par: parenthesis statement


Rep -> Rep || Cat&star
Rep -> Cat&star
Cat&star -> Cat&star Star 
Cat&star -> Star
Star -> Star *
Star -> Par
Par -> (Rep)
Par -> char


i prefer LR parsing,but it's so boring to write by hand...
besides, the grammer can be LL(1).
-------------------------------------------------------
A: Rep , B:Cat&star , C:Star, D:Par

A -> BA'
A' -> || BA' | nil
B -> CB'
B' -> CB' | nil
C -> DC'
C' -> *C' | nil
D -> (A)
D -> char

## use \\ replace || in the followed table
|   |    | 0         | 1         | 2       | 3         | 4        | 5         |
|   |    | \\        | *         | (       | )         | char     | $         |
|---+----+-----------+-----------+---------+-----------+----------+-----------|
| 0 | A  |           |           | A ->BA' |           | A -> BA' |           |
| 1 | A' | A'->\\BA' |           |         | A' -> nil |          | A' -> nil |
| 2 | B  |           |           | B->CB'  |           | B -> CB' |           |
| 3 | B' | B'-> nil  |           | B'->CB' | B'->nil   | B'->CB'  | B'->nil   |
| 4 | C  |           |           | C->DC'  |           | C->DC'   |           |
| 5 | C' | C'->nil   | C' -> *C' | C'->nil | C'->nil   | C'->nil  | C'->nil   |
| 6 | D  |           |           | D->(A)  |           | D->char  |           |

 *)



(* util functions below *)
type edge = Ch of char | NilCh
type node  = Cat of node * edge * node
	   | Or of node * node
	   | Star of node
	   | NilNode




(*   util functions above *)
open Lex

exception ParseError of string
exception Acc
	    
let slex_flush obj =
  try lex_flush obj with
    EndOfLexObj -> (Meta '$', obj)
let debug = false
let debugInfo lObj func=
  if debug then begin
      let (lVal, _newlObj) = slex_flush lObj in
      begin 
	match lVal with
	  Meta c -> Printf.printf "Meta %c " c
	| Id c -> Printf.printf "Id %c " c
	| _ -> failwith "impossible"
      end;
      print_string ("in func: " ^ func ^ "\n")
    end
  else ()
		     
let rep = ref (fun (a:lexObj) (nd:node) -> failwith "not complete yet")
				     
let rec par (lObj:lexObj) (nd:node): lexObj*node =
  let () = debugInfo lObj "par" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    (* (6, 2) *)
    Meta '(' -> begin
      let () = print_string " ( " in
      let (retObj, retNd) = !rep newlObj nd in
      let (lVal2, newlObj3) = slex_flush retObj in
      if lVal2 = Meta ')' then begin
	  print_string " ) ";
	  (newlObj3, retNd)
	end 
      else raise (ParseError "[par]")
    end
  | Id char -> begin
      Printf.printf " %c " char;
      (newlObj, (Cat (NilNode, Ch char, NilNode)))
    end
  | _ -> raise (ParseError "[par]")
	       
let rec star' (lObj:lexObj) (nd:node): lexObj*node =
  let () = debugInfo lObj "star'" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    (* (5, [02345]) *)
    Meta '|'
  | Meta '('
  | Meta ')'
  | Meta '$'
  | Id _ -> (lObj, nd)
  (* (5, 1) *)
  | Meta '*' -> begin
      print_string " * ";
      star' newlObj (Star nd)
    end
  | _ -> raise (ParseError "[star']")

let rec star (lObj:lexObj) (_nd:node): lexObj*node =
  let () = debugInfo lObj "star" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (4, 2) or (4, 4) *)
    Meta '('
  | Id _ -> begin
      let (retObj, retNd) = par lObj NilNode in
      star' retObj retNd
    end
  | _ -> raise (ParseError "[star]")
		 
let rec catAndStar' (lObj:lexObj) (nd:node): lexObj*node =
  let () = debugInfo lObj "catAndStar'" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (3, 0) or (3, 3) or (3, 5) *)
    Meta '|'
  | Meta ')'
  | Meta '$' -> (lObj, nd)
  (* (3, 2) or (3, 4) *)
  | Id _
  | Meta '(' -> begin
      let (retObj, retNd) = star lObj NilNode in
      catAndStar' retObj (Cat (nd, NilCh, retNd))
    end
  | _ -> raise (ParseError "[catAndStar']")
		 
let rec catAndStar (lObj:lexObj) (_nd:node): lexObj*node =
  let () = debugInfo lObj "catAndStar" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (2, 2) or (2, 4) *)
    Id _
  | Meta '(' -> begin
      let (retObj, retNd) = star lObj NilNode in
      catAndStar' retObj retNd 
    end
  | _ -> raise (ParseError "[catAndStar]")
		 
let rec rep' (lObj:lexObj) (nd:node): lexObj*node =
  let () = debugInfo lObj "rep'" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    (* (1, 0) *)
    Meta '|' -> begin
      let () = print_string " | " in
      let (retObj, retNd) = catAndStar newlObj NilNode in
      rep' retObj (Or (retNd, nd))
    end
  (* (1, 3) or (1, 5) *)
  | Meta '$'
  | Meta ')' -> (lObj, nd)
  | _ -> raise (ParseError "[rep']")
		 
let rec _rep (lObj:lexObj) (_nd:node): lexObj*node=
  let () = debugInfo lObj "rep" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (0, 2) or  (0, 4) *)
    Meta '('
  | Id _ -> begin
      let (retObj, retNd) = catAndStar lObj NilNode in
      rep' retObj retNd
    end
  | _ -> raise (ParseError "[rep]")
;;
rep := _rep	       

	       
let rec theEnd (lObj:lexObj) (nd:node) : lexObj*node =
  let () = debugInfo lObj "theEnd" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    Meta '$' -> begin
      print_string " $ ";
      (newlObj, nd)
      (* raise Acc *)
    end
  | _ -> begin
      let (retObj, retNd) = !rep lObj nd in
      theEnd retObj retNd
    end


