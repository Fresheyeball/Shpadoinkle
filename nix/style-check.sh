#!/usr/bin/env nix-shell
#! nix-shell lint.nix -i bash

HS_FILES=$(find . -type f -name '*.hs')
stylish-haskell $HS_FILES --inplace

# Exist with non zero status if we're now in unclean state
if [ -z "$(git status --porcelain)" ]; then
    echo "No style errors detected."
else
    echo "Style errors detected:"
    git --no-pager diff
    exit 1
fi
