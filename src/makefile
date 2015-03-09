
all: lex parser runmatch2
	@echo "create all done"

lex: lex.ml lex.mli
	ocamlc -c lex.mli
	ocamlc -c lex.ml

parser: simple_parser.ml simple_parser.mli lex.cmo
	ocamlc -c simple_parser.mli
	ocamlc -c simple_parser.ml
runmatch2: runmatch2.ml runmatch2.mli
	ocamlc -c runmatch2.mli
	ocamlc -c runmatch2.ml
clean:
	-rm *.cm*

clean_all: clean
	@echo "clean_all done"

test:
	cd tests/lex;make
	cd tests/parser;make
	cd tests/match;make
