PREFIX?=/usr

.PHONY: build
build:
	dune build

.PHONY: test
test:
	dune test

.PHONY: install
install:
	dune install --sections=bin,doc --prefix=$(PREFIX)

.PHONY: uninstall
uninstall:
	dune uninstall --sections=bin,doc --prefix=$(PREFIX)
