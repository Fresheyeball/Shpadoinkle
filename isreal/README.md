# Isreal Swan

[![Goldwater](https://gitlab.com/platonic/shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/platonic/shpadoinkle)
[![Haddock](https://img.shields.io/badge/haddock-master-informational)](https://shpadoinkle.org/isreal)
[![GPL-3](https://img.shields.io/badge/License-GPL%203--Clause-blue.svg)](https://opensource.org/licenses/gpl-3.0.html)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-isreal.svg)](https://hackage.haskell.org/package/Shpadoinkle-html)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-isreal.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-html)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-isreal/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-html)

Snowman as a service

> If you build me a snowman, then I'll build one for you.

This service is hosted at https://isreal.shpadoinkle.org. It allows you to post
Shpadoinkle code and see the resulting UI in your browser. For example, let's say we have
the local file `Hello.hs`, containing:

```haskell
module Main where


import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html


view :: () -> Html m ()
view _ = "hello world"


main :: IO ()
main = runJSorWarp 8080 $ simple runParDiff () view getBody
```

You can send this to Isreal Swan with the following `curl` command:

```bash
curl -X POST -H "Content-Type:application/octet-stream" --data-binary @Hello.hs \
  https://isreal.shpadoinkle.org/compile/hello-token?nonce=0
```

You should increment the nonce for browser cache busting.

Notice **hello-token** in the URL. It's _on you_ to make this a unique token for your work,
as the system is open-ended (any arbitrary (uri encoded) string is fine).
Reusing this token will result in incremental rebuilds, which
are much faster. Also please note, these spaces are ephemeral and will be deleted.

The `curl` command will respond with `Either` an error message from the compiler or
a message indicating success. If your code compiled successfully, you will be able to see
the resulting UI here:

`https://isreal.shpadoinkle.org/serve/hello-token/index.html`


## Deps & Such

Many common Haskell dependencies are provided.
The environment is based on the cabal file located [here](https://gitlab.com/platonic/shpadoinkle/-/blob/master/isreal/swan/swan.cabal).

## Clean Up

Clean up your token:

```bash
curl -X DELETE http://localhost:8080/clean/hello-token
```

## GPL License

Shpadoinkle Isreal (all files within this directory), are provided under the terms of the GNU
General Public License v3.0
