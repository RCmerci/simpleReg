type lexVal = Id of char
	    | Meta of char
	    | BackSlash

type lexObj = lexVal list

exception LexError of string
exception EndOfLexObj
			
let stringMap ~(f: int -> 'b -> char -> 'a*'b) ~(init: 'b) (s: string) : 'a list =
  let _doc = "function like List.map on string, 
	     in addition, [f] 's 2st parament is info from the last call on [f]" in
  let res = ref []
  and info = ref init in
  let aux ind c =
    let (f_res, f_info) = f ind !info c in
    res := f_res :: !res;
    info := f_info;
    ()
  in
  Bytes.iteri aux s;
  List.rev !res
  

type bsFlag = [`True | `False] (* backslash flag *)
		     
let lex_compile (regexStr: string) : lexObj =
  let aux ind (info: bsFlag) ch : lexVal*bsFlag=
    let _doc = "[info] should be one of [`True | `False]" in
    match (info, ch) with
      (`False , '(') -> (Meta '(', `False)
    | (`True , '(') -> (Id '(', `False)
    | (`False , ')') -> (Meta ')', `False)
    | (`True , ')') -> (Id ')', `False)
    | (`False , '*') -> (Meta '*', `False)
    | (`True , '*') -> (Id '*', `False)
    | (`False , '+') -> (Meta '+', `False)
    | (`True , '+') -> (Id '+', `False)
    | (`False , '?') -> (Meta '?', `False)
    | (`True , '?') -> (Id '?', `False)
    | (`False , '[') -> (Meta '[', `False)
    | (`True , '[') -> (Id '[', `False)
    | (`False , ']') -> (Meta ']', `False)
    | (`True , ']') -> (Id ']', `False)
    (* | (`False , '{') -> (Meta '{', `False)  
                            nowadays,not support something like"{1,3}"*)
    (* | (`True , '{') -> (Id '{', `False) *)
    (* | (`False , '}') -> (Meta '}', `False) *)
    (* | (`True , '}') -> (Id '}', `False) *)
    | (`False , '|') -> (Meta '|', `False)
    | (`True , '|') -> (Id '|', `False)
    | (`False , '.') -> (Meta '.', `False) (* . for any character but newline*)
    | (`True , '.') -> (Id '.', `False)
    | (`True , 'd') -> (Meta 'd', `False)  (* \d for digit *)
    | (`True , 's') -> (Meta 's', `False)  (* \s for whitespace character *)
    | (`False , '\\') -> (BackSlash, `True) 
    | (`True , '\\') -> (Id '\\', `False)
    | (`False , c) -> (Id c, `False)
    | (`True , c) -> raise (LexError ("at:"^string_of_int ind))
  in
  let l = stringMap aux `False regexStr in
  List.filter (function BackSlash -> false | _ -> true) l


let lex_flush (obj: lexObj) : lexVal*lexObj =
  match obj with
    hd :: tl -> (hd, tl)
  | [] -> raise EndOfLexObj
			  
