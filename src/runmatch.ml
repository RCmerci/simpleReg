type edge = Ch of char 
type obj  = Cat of obj * edge * obj
	  | Or of obj * obj
	  | Star of obj
	  | NilObj

let testcase = ['a';'a';'b']
(*a*b|c*)
let testreg = Or (Cat (Star (Ch 'a')
			    Ch 'b',
		       NilObj),
		  Cat (NilObj,
		       Ch 'c',
		       NilObj))


let rec run (o:obj) = function
    [] -> []
  | hd::tl -> begin
      match o with
	NilObj -> hd::tl
      | Cat (NilObj,Ch e, o2) -> begin
	  if e = hd then run o2 tl
	  else 
	end
      | Cat (o1, _, _) -> begin
	  
	end
    end
		
let 
