FILES = opam install.sh META.patch
PATCHES = $(FILES:=.patch)

.PHONY: all
all: $(PATCHES)

%.patch: %.modified
	diff -up original/$(<:.modified=) $< > $@; [ $$? -eq 1 ] || exit 0

.PHONY: clean
clean:
	rm *.patch
