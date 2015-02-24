exception ParseError of string
exception Acc
val rep : (Lex.lexObj -> Lex.lexObj) ref
val par : Lex.lexObj -> Lex.lexObj
val star' : Lex.lexObj -> Lex.lexObj
val star : Lex.lexObj -> Lex.lexObj
val catAndStar' : Lex.lexObj -> Lex.lexObj
val catAndStar : Lex.lexObj -> Lex.lexObj
val rep' : Lex.lexObj -> Lex.lexObj
val _rep : Lex.lexObj -> Lex.lexObj
val theEnd : Lex.lexObj -> Lex.lexObj
