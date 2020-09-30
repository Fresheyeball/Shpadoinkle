# Shpadoinkle Servant Router

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![Haddock](https://img.shields.io/badge/haddock-master-informational)](https://shpadoinkle.org/router)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-router.svg)](https://hackage.haskell.org/package/Shpadoinkle-router)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-router.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-router)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-router/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-router)


A Servant combinator-based router for Shpadoinkle single-page applications.
Consuming this router requires that you provide two types:

- Type alias for the recognized URIs
- ADT representing views that can be rendered

The relationship between these two types is surjective. meaning more than one URI
may result in the same route. This is important for backward compatibility, so the
routing schema can evolve while still supporting older schemas.

Because interactions are done through the ADT, application code should be type-safe,
and route canonically.

```haskell
-- The accepted URIs
type SPA
  =            "echo" :> QueryParam "echo" Text :> Raw
  :<|> "v2" :> "echo" :> QueryParam "echo" Text :> Raw
  :<|> "home" :> Raw

-- The routes that can be rendered
data Route
  = Echo (Maybe Text)
  | Home

-- Surjection from URIs to routes
routes :: SPA :>> Route
routes
  =    REcho
  :<|> REcho
  :<|> Home

-- Canonical URI for each route
instance Routed SPA Route where
  redirect = \case
    REcho t -> Redirect (Proxy @("v2" :> "echo" :> QueryParam "echo" Text :> Raw)) ($ t)
    Home    -> Redirect (Proxy @("home" :> Raw)) id
```

The above specification can be used on both the client and the server. See the `servant-crud` example for more on how to use this technique.
