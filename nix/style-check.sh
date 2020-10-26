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
    echo "\n"
    echo "\tPlease run nix/style-check.sh to auto format the code and commit changes."
    exit 1
fi
