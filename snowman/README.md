# Snowman

[![Goldwater](https://gitlab.com/platonic/shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/platonic/shpadoinkle)
[![GPL-3](https://img.shields.io/badge/License-GPL%203--Clause-blue.svg)](https://opensource.org/licenses/gpl-3.0.html)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)

☃️  ⟶ 🥔 ☃️

### We can make him tall. Or we can make him not so tall...

[Snowman](https://youtu.be/JQ1ZOFNBL68?t=8) is a seed project for [Shpadoinkle](https://gitlab.com/platonic/Shpadoinkle/-/tree/master/#shpadoinkle).

## Get the project

To create a new Snowman project:

```bash
bash <( curl https://gitlab.com/platonic/shpadoinkle/-/raw/master/snowman/generate.sh )
```

This project supports building with both `nix` and `stack`, but requires `nix` to install.

## Stack

Stack should work out of the box, provided you have the following installed on your system:

- git
- zlib
- zlib-dev
- pcre

```bash
stack build
```

## Nix

This project is built with [Nix](https://nixos.org/). [Visit the Shpadoinkle guide for Nix.](https://shpadoinkle.org/docs/getting-started/index.html#_nix)

## Build the project

The included `default.nix` file has some arguments to customize your build. To build with GHC

```bash
nix-build
```

To build with GHCjs

```bash
nix-build --arg isJS true
```

## Develop

```
nix-shell
```

Will drop you into a dev shell with [Ghcid](https://github.com/ndmitchell/ghcid#ghcid----) and other common haskell development tools.

### TLDR

Get a ghcid server with live reloads in one line

```bash
nix-shell --run "ghcid --command 'cabal repl' -W -T Main.dev"
```

Get a hoogle server in one line

```bash
nix-shell --arg withHoogle true --command "hoogle serve"
```

## GPL and CC0 Licenses

Shpadoinkle Snowman (all files within this directory), are provided under the terms of the GNU
General Public License v3.0.

However, the code generated by Shpadoinkle Snowman is licensed under the terms of the
Creative Commons Zero License. Which implies "No Rights Reserved".
