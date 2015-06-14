
.SECONDARY:
.PHONY: test clean

OCB = ocamlbuild -use-ocamlfind -quiet
SOURCES = dataTypes.ml expressions.ml typeChecker.ml parser.mly lexer.mll normalize.ml
TESTS = $(addprefix test/, $(addsuffix .native, testParser testTypeChecker testPrettyPrinter testNormalizer))

test: $(SOURCES) $(TESTS)
	./testParser.native
	./testTypeChecker.native
	./testNormalizer.native

%.native: %.ml $(SOURCES)
	$(OCB) $@

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o && ocamlbuild -clean
