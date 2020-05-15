# Shpadoinkle

[![ZERO](http://fresheyeball.com/doinkle.svg)](https://www.youtube.com/watch?v=0CizU8aB3c8)

[![pipeline status](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle/commits/master)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)

### I think I know precisely what I mean...

Shpadoinkle is a UI programming model focused on simplicity and performance.

Currently builds and runs with **ghc 8.6.x** and **ghcjs 8.6.x**

#### [See the TODOMVC example here.](http://fresheyeball.gitlab.io/Shpadoinkle/)

## Hackage Package Matrix

| Name | Status |
|---|---|
| Core | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle.svg)](https://hackage.haskell.org/package/Shpadoinkle) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle) |
| Html | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-html.svg)](https://hackage.haskell.org/package/Shpadoinkle-html) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-html.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-html) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-html/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-html) |
| Static | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-static.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-static) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-static.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-static) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-static/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-static) |
| ParDiff | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-pardiff.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-pardiff) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-pardiff.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-pardiff) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-pardiff/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-pardiff) |
| Snabbdom | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-snabbdom.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-snabbdom) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-snabbdom.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-snabbdom) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-snabbdom/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-snabbdom) |
| Router | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-router.svg)](https://hackage.haskell.org/package/Shpadoinkle-router) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-router.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-router) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-snabbdom/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-snabbdom) |
| Widgets | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-widgets.svg)](https://hackage.haskell.org/package/Shpadoinkle-widgets) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-widgets.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-widgets) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-widgets/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-widgets) |
| Examples | [![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-examples.svg)](https://hackage.haskell.org/package/Shpadoinkle-examples) [![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-examples.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-examples) [![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-widgets/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-widgets) |

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
