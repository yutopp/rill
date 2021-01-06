PREFIX?=/usr

.PHONY: build
build:
	opam exec -- dune build

.PHONY: test
test:
	opam exec -- dune test

.PHONY: install
install:
	opam exec -- dune install --sections=bin,doc --prefix=$(PREFIX)

.PHONY: uninstall
uninstall:
	opam exec -- dune uninstall --sections=bin,doc --prefix=$(PREFIX)
