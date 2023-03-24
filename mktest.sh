#!/usr/bin/env bash

set -eu

test $# -eq 1 || { echo "Usage: $0 <testname>"; exit 1; }
test -f "./tests/$1.nhtml" && { echo "Test $1 already exists"; exit 1; }

"$EDITOR" "./tests/$1.nhtml" || exit 1
"$EDITOR" "./tests/expected/$1.html" || { rm "./tests/$1.nhtml"; exit 1;}
