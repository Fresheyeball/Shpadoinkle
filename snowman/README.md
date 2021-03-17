# Snowman

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
nix-shell --command "ghcid --command 'cabal repl' -W -T Main.dev"
```

Get a hoogle server in one line

```bash
nix-shell --arg withHoogle true --command "hoogle serve"
```

### License

The code to generate a snowman is BSD licensed like the rest of this project. However the code generated on your local machine is CC0.
