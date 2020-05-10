# Shpadoinkle Backend Static

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-static.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-static)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-static.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-static)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-static/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-static)

This module provides a static rendering backend that translates Shpadoinkle `Html` into `Text`.

For example

```haskell
page :: Html' a
page = h "div" [ ("class", PText "header" ) ]
  [ h "h1" [] [ text "Trappers!" ] ]

main = putStrLn $ unpack $ renderStatic page
```

will output

```html
<div class="header"><h1>Trappers!</h1></div>
```
