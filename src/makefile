
all: lex parser
	@echo "create all done"

lex: lex.ml lex.mli
	ocamlc -c lex.mli
	ocamlc -c lex.ml

parser: simple_parser.ml simple_parser.mli lex.cmo
	ocamlc -c lex.cmo simple_parser.mli
	ocamlc -c lex.cmo simple_parser.ml
clean:
	-rm *.cm*

clean_all: clean
	@echo "clean_all done"
