# Shpadoinkle

[![pipeline status](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle/commits/master)
[![built with nix](https://builtwithnix.org/badge.svg)](https://builtwithnix.org)

### I think I know precisely what I mean...

Shpadoinkle is a UI programming model focused on simplicity and performance.

Currently builds and runs with **ghc 8.4.x** and **ghcjs 8.4.x**

[See the TODOMVC example here.](http://fresheyeball.gitlab.io/Shpadoinkle/)

## Building the project

Like most GHCjs projects, this is build with [Nix](https://nixos.org/)

### build ghcjs version

```bash
nix-build
```

### build ghc version

```bash
nix-build --argstr compiler ghc843
```