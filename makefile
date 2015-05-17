
.SECONDARY:

all: test_parser.native

test_parser.native: expressions.ml parser.mly lexer.mll test_parser.ml
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core test_parser.native

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o
