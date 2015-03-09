open Runmatch2
open Simple_parser

let testcases = [
    ("abc", "abc");
    ("abbbb", "ab*");
    ("abcbc", "a(bc)*");
    ("yyyycccc", "y*cccc");
    ("holy shit", "holy shit");
    ("aaaabbababaabb", "(aa)*(b*aba|b*a*b*)*");
  ]

let run () =
  let f s = let a = Lex.lex_compile s in let (_, b) = theEnd a NilNode in b in
  let aux (str, regex) : bool =
    try run str (f regex) with
      Stack_overflow -> false
  in
  let rec print_bool_list l ind=
    match l with
      [] -> ()
    | true :: tl -> Printf.printf "%d: success" ind;print_newline();
		    succ ind |> print_bool_list tl
    | false :: tl -> Printf.printf "%d: fail" ind;print_newline();
		     succ ind |> print_bool_list tl
  in
  let () = print_bool_list (List.map aux testcases) 0 in
  List.map aux testcases
  |> List.exists (fun a -> a=false)
  |> (fun a -> if a then print_string "test fail!" else print_string "test success")


let () = run ()
