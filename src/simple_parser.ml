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
type edge = Ch of char | Nil
type obj  = Cat of obj * edge * obj
	  | Or of obj * obj * obj * obj
	  | Star of obj * obj * obj
	  | NilObj
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
		     
let rep = ref (function (a:lexObj) -> failwith "not complete yet")
				     
let rec par (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "par" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    (* (6, 2) *)
    Meta '(' -> begin
      let () = print_string " ( " in
      let retObj = !rep newlObj in
      let (lVal2, newlObj3) = slex_flush retObj in
      if lVal2 = Meta ')' then begin
	  print_string " ) ";
	  newlObj3
	end 
      else raise (ParseError "[par]")
    end
  | Id char -> begin
      Printf.printf " %c " char;
      newlObj
    end
  | _ -> raise (ParseError "[par]")
	       
let rec star' (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "star'" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
    (* (5, [02345]) *)
    Meta '|'
  | Meta '('
  | Meta ')'
  | Meta '$'
  | Id _ -> lObj
  (* (5, 1) *)
  | Meta '*' -> begin
      print_string " * ";
      star' newlObj
    end
  | _ -> raise (ParseError "[star']")

let rec star (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "star" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (4, 2) or (4, 4) *)
    Meta '('
  | Id _ -> begin
      let retObj = par lObj in
      star' retObj
    end
  | _ -> raise (ParseError "[star]")
		 
let rec catAndStar' (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "catAndStar'" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (3, 0) or (3, 3) or (3, 5) *)
    Meta '|'
  | Meta ')'
  | Meta '$' -> lObj
  (* (3, 2) or (3, 4) *)
  | Id _
  | Meta '(' -> begin
      let retObj = star lObj in
      catAndStar' retObj
    end
  | _ -> raise (ParseError "[catAndStar']")
		 
let rec catAndStar (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "catAndStar" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (2, 2) or (2, 4) *)
    Id _
  | Meta '(' -> begin
      let retObj = star lObj in
      catAndStar' retObj
    end
  | _ -> raise (ParseError "[catAndStar]")
		 
let rec rep' (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "rep'" in
  let (lVal, newlObj) = slex_flush lObj in
  match lVal with
(* (1, 0) *)
    Meta '|' -> begin
      let () = print_string " | " in
      let retObj = catAndStar newlObj in
      rep' retObj
    end
  (* (1, 3) or (1, 5) *)
  | Meta '$'
  | Meta ')' -> lObj
  | _ -> raise (ParseError "[rep']")
		 
let rec _rep (lObj:lexObj) : lexObj=
  let () = debugInfo lObj "rep" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    (* (0, 2) or  (0, 4) *)
    Meta '('
  | Id _ -> begin
      let retObj = catAndStar lObj in
      rep' retObj 
      end
  | _ -> raise (ParseError "[rep]")
;;
rep := _rep	       

	       
let rec theEnd (lObj:lexObj) : lexObj =
  let () = debugInfo lObj "theEnd" in
  let (lVal, _newlObj) = slex_flush lObj in
  match lVal with
    Meta '$' -> begin
      print_string " $ ";
      raise Acc
    end
  | _ -> begin
      let retObj = !rep lObj in
      theEnd retObj
    end


