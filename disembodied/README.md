# Shpadoinkle Disembodied

[![Goldwater](https://gitlab.com/platonic/shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/platonic/shpadoinkle)
[![Haddock](https://img.shields.io/badge/haddock-master-informational)](https://shpadoinkle.org/disembodied)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-disembodied.svg)](https://hackage.haskell.org/package/Shpadoinkle-disembodied)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-disembodied.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-disembodied)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-disembodied/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-disembodied)

This module provides static site generation tools for Html.

## Usage

Lets say you have the following API routes for you SPA application:

```haskell
type Pages m
  = "about" :> View m Int
  :<|> View m ()
```

And you have a view for each. You can now produce a `SiteSpec` mapping these routes to the views.

```haskell
site :: SiteSpec () (Pages m)
site = about 0 :<|> const home
```

Which can be written to static pages with `writeSite`. Each route will become a directory,
and each View will become an `index.html` file in that directory.
