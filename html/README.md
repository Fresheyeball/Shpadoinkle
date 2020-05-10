# Shpadoinkle Html

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-html.svg)](https://hackage.haskell.org/package/Shpadoinkle-html)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-html.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-html)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-html/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-html)

This module provides named functions for generating Html, and other browser utilities.

For example, instead of writing

```haskell
view = h "div" [ ("class", PText "foo") ]
  [ h "span" [] [ text "hi there" ] ]
```

we can write

```haskell
view = div "foo" [ span_ [ "hi there" ] ]
```

which is a bit nicer, and eleminates the risk of typeos in tag names. It also
provies some nice `IsString` instances for ergonomics.

## Keyboard

This module provides pattern synonyms for common key codes. For example:

```haskell
div [ onKeyup $ \case
    Enter -> fireLazors
    UpArrow -> jump
    DownArrow -> crouch
    LeftArrow -> move -1
    RightArrow -> move 1
  ]
```

which is a bit nicer, and harder to get wrong than using magic int's to
identify keys.

## Browser utilities

We provide high level apis to lower level browser apis. Including

- Local Storage

I know it's just one right now.
But this is the place to contribute more. Such as

- Scroll Position
- XHR Requests
- Visibility API
- Notifications
- ect...


