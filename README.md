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

### Developing with GHCID

This is a multi-package mutli-target project. Each of the top level directories is it's own package,
so to run ghcid we need to be in specify which package and which target we want to compile.

**TLDR one liner:**

```bash
cd examples && nix-shell --command "cd .. && ghcid --command 'cabal repl examples:counter'" || cd ..
```


**Here is the current best path to getting ghcid up an running:**

Let's say we wanted to run ghcid for the counter example...

```bash
cd examples
# sets up dependencies for the examples package
nix-shell
# gets us back to cabal.project root
cd ..
# run the counter target of the examples package
ghcid --command "cabal repl examples:counter"
```

If you wanted to run ghcid for core...

```bash
cd core
# sets up dependencies for the core package
nix-shell
# gets us back to cabal.project root
cd ..
# run the library target of the core package
ghcid --command "cabal repl core"
```

Protip:
The `examples` package's dependencies are a superset of the other packages. So you can easily switch targets like so

```bash
cd examples
nix-shell
cd ..
ghcid --command "cabal repl examples:counter"
# run against core next
ghcid --command "cabal repl core"
# run against html as well
ghcid --command "cabal repl html"
```
