
.SECONDARY:

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.ll: %.scm
	guile -e main -s lulz.scm $^ $@

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o
