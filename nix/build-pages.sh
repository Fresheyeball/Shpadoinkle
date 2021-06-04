#!/bin/sh

echo "Building examples with GHCjs"
nix-build --arg isJS true -A Shpadoinkle-examples --fallback

echo "Copying in examples"
mkdir -p public/examples
cp -r result/bin/*.jsexe public/examples
rm result*

echo "Building Haddocks and copying in"
./nix/fish.sh
rm result*

echo "Proving to Keybase"
cp website/keybase.txt public/keybase.txt

echo "Building Website"
nix-build website --fallback --arg ci true

echo "Copying in Website"
cp --dereference -r result/* public
rm result*

chmod -R 755 public/*
