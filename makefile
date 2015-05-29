
.SECONDARY:

all: testParser.native testTypeChecker.native

test: all
	./testParser.native
	./testTypeChecker.native

testParser.native: expressions.ml parser.mly lexer.mll testParser.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core testParser.native

testTypeChecker.native: expressions.ml parser.mly lexer.mll typeChecker.ml testTypeChecker.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core testTypeChecker.native

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o && ocamlbuild -clean
