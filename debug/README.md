# Shpadoinkle Debug

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-debug.svg)](https://hackage.haskell.org/package/Shpadoinkle-debug)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-debug.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-debug)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-debug/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-debug)

This package exposes some useful debugging functions for tracing the state of Shpadoinkle applications as they change over time. It exposes 2 type classes currently.

```haskell
class LogJS (c :: Type -> Constraint) where
  logJS :: c a => a -> JSM ()

class LogJS c => Trapper c where
  trapper :: c a => JSContextRef -> a -> a
```

These are intended to by used with `TypeApplications`, so you can choose the means of conversion before passing to `console.log`. For example:

```haskell
main :: IO ()
main = runJSorWarp 8080 $ do
  ctx <- askJSM
  simple runParDiff initial (view . trapper @ToJSON ctx) getBody
```

This will log all state by first encoding to JSON with Aeson, then then logging with `JSON.parse` so the browser console has the nice native display. If we change it to `trapper @Show ctx` it will use the `Show` instance instead.
