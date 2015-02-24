type lexVal = Id of char
	    | Meta of char
	    | BackSlash

type lexObj			

exception LexError of string
exception EndOfLexObj       
val lex_compile : string -> lexObj
			     
val lex_flush : lexObj -> lexVal*lexObj
