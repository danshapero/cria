
.SECONDARY:

%.ll: %.c
	clang -O0 -S -emit-llvm $<

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o
