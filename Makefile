all:
	@echo try "make run-1" or "make run-2"

%.out: euler/%.ml
	@ocamlc -w -24 -o $@ $<

run-%: %.out
	@./$<
	@rm -f $<

.PHONY: neat run-neat
neat: tail-call not-optimized
tail-call: neat/tail-call.ml
	@ocamlc -w -24 -o $@ $<
not-optimized: neat/not-optimized.ml
	@ocamlc -w -24 -o $@ $<

run-neat: neat
	@echo At first glance, performance seems similar in terms of cpu numbers.
	@echo However, tail call optimized one can handle a lot more before stack
	@echo overflow happens.
	@echo Note: this is slow, but it will finish while the other version fails
	-./not-optimized 1000000
	-./tail-call     1000000000

clean:
	rm -f tail-call
	rm -f not-optimized
	rm -f *.out