# Rillc

## How to develop

### Initial setup

```shell
opam repo add rillc-deps-opam-repo https://github.com/yutopp/rillc-deps-opam-repo.git
```

### Update packages

```shell
opam update
opam install . --deps-only --with-test -y
```

### Build executables

```shell
make build
```

### Testing

```shell
make test
```
