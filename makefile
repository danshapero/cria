
.SECONDARY:
.PHONY: test clean

OCB = ocamlbuild -use-ocamlfind -quiet -Is src,src/syntactic,src/semantic,test
TESTS = $(addsuffix .native, testParser testTypeChecker testPrettyPrinter testNormalizer testFindFreeVars testRenaming testNExpr)

test: $(TESTS)
	./testParser.native
	./testTypeChecker.native
	./testNormalizer.native
	./testRenaming.native
	./testFindFreeVars.native

%.native:
	$(OCB) $@

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o && ocamlbuild -clean
