type edge = Ch of char | NilCh
type node =
    Cat of node * edge * node
  | Or of node * node
  | Star of node
  | NilNode
exception ParseError of string
(* exception Acc *)
(* val slex_flush : Lex.lexObj -> Lex.lexVal * Lex.lexObj *)
(* val debug : bool *)
(* val debugInfo : Lex.lexObj -> string -> unit *)
(* val rep : (Lex.lexObj -> node -> Lex.lexObj * node) ref *)
(* val par : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val star' : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val star : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val catAndStar' : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val catAndStar : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val rep' : Lex.lexObj -> node -> Lex.lexObj * node *)
(* val _rep : Lex.lexObj -> node -> Lex.lexObj * node *)
val theEnd : Lex.lexObj -> node -> Lex.lexObj * node
