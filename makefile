
.SECONDARY:

all: testParser.native

testParser.native: expressions.ml parser.mly lexer.mll testParser.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core testParser.native

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o && ocamlbuild -clean
