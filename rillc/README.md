# Rillc

Rillc is a compiler program and a main component for Rill programming language.

Building and testing of `rillc` can be done independently under this directory.

## How to develop

We recommend installing these packages for development.

```
opam install merlin ocamlformat
```

### Initial setup

```shell
opam repo add rillc-deps-opam-repo https://github.com/yutopp/rillc-deps-opam-repo.git
opam update
opam install . --deps-only --no-depexts --with-test -y --locked
```

### (optional) Update packages

```shell
opam update
opam install . --deps-only --no-depexts --with-test -y
opam lock .
```

### Build executables

```shell
make build
```

### Testing

```shell
make test
```
