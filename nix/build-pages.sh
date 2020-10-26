#!/bin/sh

echo "Building examples with GHCjs"
nix-build --arg isJS true -A Shpadoinkle-examples --fallback

echo "Copying in examples"
mkdir -p public/examples
cp -r result/bin/*.jsexe public/examples
rm result*

echo "Building Antora docs"
nix-build docs --fallback

echo "Copying in Antora docs"
cp -r result/* public
rm result*

echo "Building Haddocks and copying in"
./nix/fish.sh
rm result*

# echo "Building Marketing"
# nix-build marketing --fallback

# echo "Copying in Marketing"
# cp -r result/* public
# rm result*

chmod -R 755 public/*
