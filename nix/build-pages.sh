#!/bin/sh

nix-build --arg isJS true -A Shpadoinkle-examples --fallback

mkdir -p public/examples
cp -r result/bin/*.jsexe public/examples
rm result*

nix-build docs --fallback

cp -r result/* public
./nix/fish.sh
chmod -R 755 public/*
