
.SECONDARY:

%.bc: %.ll
	llvm-as $<

%.s: %.bc
	llc $<

clean:
	rm -rf *.s *.bc *.o
