# Shpadoinkle Template

[![Goldwater](https://gitlab.com/platonic/shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/platonic/shpadoinkle)
[![Haddock](https://img.shields.io/badge/haddock-master-informational)](https://shpadoinkle.org/template)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-template.svg)](https://hackage.haskell.org/package/Shpadoinkle-template)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-template.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-template)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-template/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-template)

This module provides the ability to read files into Shpadoinkle views.

## Usage

Lets say you have `template.html`

```html
<h1>Hi!</h1>
<div>Nice to meat you</div>
```

you can now embed it into a Shpadoinkle

```haskell
view :: Html m a
view = div [ className "my-view" ] $(embedHtml "./template.html")
```


which will render as

```html
<div class="my-view">
  <h1>Hi!</h1>
  <div>Nice to meat you</div>
</div>
```
