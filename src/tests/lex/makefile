## lex test makefile


VPATH = ../../

test: test_lex.cmo lex.cmo
	ocamlc -I ../.. -o test lex.cmo test_lex.cmo
	./test

test_lex.cmo: test_lex.ml lex.cmo
	ocamlc -I ../.. -c test_lex.ml

lex.cmo: lex.ml lex.mli
	cd ../..;pwd;make lex

clean:
	-rm *.cm*

cleanall: clean
	-rm test
