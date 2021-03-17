#!/usr/bin/env bash

set -eu

if ! command -v nix-build &> /dev/null
then
  echo "Nix is required for this to work"
fi

tmpdir=$(mktemp -d "${TMPDIR:-/tmp}"/tmp.XXXXXXXX)
git clone https://gitlab.com/platonic/shpadoinkle.git $tmpdir
$(nix-build $tmpdir -A swan --no-out-link --show-trace)/bin/swan
rm -rf $tmpdir
