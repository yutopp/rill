 #!/bin/bash
set -e -o pipefail

eval `opam config env`

# make report
bisect-ppx-report -I rillc/src/ -html /artifacts/coverage/ test/bisect-*.out

# send coverage
ocveralls --repo_token $COVERALLS_REPO_TOKEN --send test/bisect-*.out --prefix rillc/src --git
