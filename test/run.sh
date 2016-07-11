 #!/bin/bash
set -e
set -o pipefail

eval `opam config env`
ls -la

omake COVERAGE=true
omake test_all

# bisect-ppx-report -I rillc/src/ -html /artifacts/coverage/ test/bisect-*.out
ocveralls --repo_token $COVERALLS_REPO_TOKEN --send test/bisect-*.out --prefix rillc/src --send --git
