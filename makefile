JBUILDER ?= jbuilder

all:
	$(JBUILDER) build @install @DEFAULT

check: tests

test:
	$(JBUILDER) runtest

clean:
	rm -rf _build

.PHONY: test all clean check
