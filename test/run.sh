 #!/bin/bash
set -e
set -o pipefail

eval `opam config env`
ls -la

omake COVERAGE=true
omake test
