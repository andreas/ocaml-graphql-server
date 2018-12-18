DUNE ?= dune

all:
	$(DUNE) build @install @DEFAULT

check: tests

test:
	$(DUNE) runtest --no-buffer

clean:
	dune clean
	rm -rf *.install

.PHONY: test all clean check
