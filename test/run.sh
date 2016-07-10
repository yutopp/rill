 #!/bin/bash
set -e
set -o pipefail

eval `opam config env`
ls -la

omake
omake test_all

bisect-ppx-report -I rillc/src/ -html /artifacts/coverage/ test/bisect-*.out
