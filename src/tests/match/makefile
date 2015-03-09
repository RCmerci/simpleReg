## runmatch2 test makefile


VPATH = ../../

test: test_runmatch2.cmo lex.cmo simple_parser.cmo runmatch2.cmo
	ocamlc -I ../.. -o test_runmatch2.cmo
	./test

test_runmatch2.cmo: test_runmatch2.ml lex.cmo simple_parser.cmo runmatch2.cmo
	ocamlc -I ../.. -c test_runmatch2.ml

lex.cmo: lex.ml lex.mli
	cd ../..;make lex

simple_parser.cmo: simple_parser.ml simple_parser.mli
	cd ../..;make parser

runmatch2.cmo: runmatch2.ml runmatch2.mli
	cd ../..;make runmatch2
