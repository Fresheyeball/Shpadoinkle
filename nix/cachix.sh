#!/usr/bin/env bash

# We want cache as a singular step. Because we do not wish to cache parts of the
# project if the set is broken.


set -eu


chan=$(cat ./nix/chan.nix | tr -d '"')


cmds=(
  "nix-build --arg isJS true"
  "nix-build --arg isJS false"
  "nix-build --arg isJS true  -A Shpadoinkle-examples.env"
  "nix-build --arg isJS false -A Shpadoinkle-examples.env"
  "nix-build --argstr system x86_64-darwin --arg isJS true"
  "nix-build --argstr system x86_64-darwin --arg isJS false"
  "nix-build --argstr system x86_64-darwin --arg isJS true  -A Shpadoinkle-examples.env"
  "nix-build --argstr system x86_64-darwin --arg isJS false -A Shpadoinkle-examples.env"
  "nix-build ./snowman/template                               --arg isJS false --arg asShell true                       --no-out-link --argstr chan $chan --argstr shpadoinkle-path $PWD"
  "nix-build ./snowman/template                               --arg isJS false --arg asShell true --arg withHoogle true --no-out-link --argstr chan $chan --argstr shpadoinkle-path $PWD"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true                       --no-out-link --argstr chan $chan --argstr shpadoinkle-path $PWD"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS true  --arg asShell true                       --no-out-link --argstr chan $chan --argstr shpadoinkle-path $PWD"
  "nix-build ./snowman/template --argstr system x86_64-darwin --arg isJS false --arg asShell true --arg withHoogle true --no-out-link --argstr chan $chan --argstr shpadoinkle-path $PWD"
)


pids=()


for cmd in "${cmds[@]}"; do
  ($cmd > /dev/null) && ($cmd | cachix push shpadoinkle && echo "^ with $cmd") &
  pids+=($!)
done


for pid in "${pids[@]}"; do
  wait "$pid"
done


echo "Cachix up to date"
