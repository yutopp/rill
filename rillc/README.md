# Rillc

Rillc is a compiler program and a main component for Rill programming language.

Building and testing of `rillc` can be done independently under this directory.

## How to develop

### Initial setup

```shell
opam switch create . --empty -y
opam repo add rillc-deps-opam-repo https://github.com/yutopp/rillc-deps-opam-repo.git
opam update
opam install . --deps-only --with-test -y --locked
```

### (optional) Update packages

```shell
opam update
opam install . --deps-only --with-test -y
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
