
.SECONDARY:

%.ll: %.c
	clang -S -emit-llvm $<

%.s: %.ll
	llc $<

clean:
	rm -rf *.s *.bc *.o
