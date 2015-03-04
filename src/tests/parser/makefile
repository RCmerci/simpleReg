##parser test makefile


VPATH = ../../

test: test_simple_parser 
	ocamlc -I ../.. -o test lex.cmo simple_parser.cmo test_simple_parser.cmo
	./test

test_simple_parser: test_simple_parser.ml lex_parser
	ocamlc -I ../.. -c test_simple_parser.ml

lex_parser: lex.ml lex.mli simple_parser.ml simple_parser.mli
	cd ../..;make lex;make parser
clean:
	-rm *.cm*
cleanall: clean
	-rm test
