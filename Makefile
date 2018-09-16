all:
	@echo try "make run-1" or "make run-2"
%.out: euler/%.ml
	@ocamlc -w -24 -o $@ $<
.PHONY: run-%
run-%: %.out
	@./$<
	@rm -f $<
