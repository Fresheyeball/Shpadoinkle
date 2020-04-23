# Shpadoinkle

[![pipeline status](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle/commits/master)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

### I think I know precisely what I mean...

Shpadoinkle is a UI programming model focused on simplicity and performance.

Currently builds and runs with **ghc 8.6.x** and **ghcjs 8.6.x**

[See the TODOMVC example here.](http://fresheyeball.gitlab.io/Shpadoinkle/)

## Building the project

Like most GHCjs projects, this is build with [Nix](https://nixos.org/)

Both builds have `--arg isJS` to chose GHC or GHCjs (defaults to false), and `--argstr compiler ghc864` to select compiler version.

### build ghcjs version

```bash
nix-build --arg isJS true
```

### build ghc version

```bash
nix-build
```


### running the todomvc example

```
nix-build --argstr compiler ghc864
./result/bin/todomvc
open http://localhost:8080
```
