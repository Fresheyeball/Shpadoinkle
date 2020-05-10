# Shpadoinkle Backend Snabbdom

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-snabbdom.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-snabbdom)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-snabbdom.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-snabbdom)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-snabbdom/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-snabbdom)

This package contains a script to setup the [Snabbdom](https://github.com/snabbdom/snabbdom) virtual dom library as a backend to render Shpadoinkle applications.

> Snabbdom consists of an extremely simple, performant, and extensible core that is only ≈ 200 SLOC. It offers a modular architecture with rich functionality for extensions through custom modules. To keep the core simple, all non-essential functionality is delegated to modules.

These design decisions made Snabbdom a good fit for Shpadoinkle's first high-performance pure JavaScript backend. Right now Snabbdom is being provided via [CloudFlare](https://cdnjs.com/) content delivery network (CDN) and is not included in this repo. This is great for getting started fast and having a transparent developer experience where you can simply switch to the backend of your choosing. However, this is not a stable long term approach as the CDN artifact could be removed at any time. If you wish to use Snabbdom in production, I recommend either:

- Proactively monitoring the CDN endpoints
- Wrap `SnabbdomT` in your own `newtype` and overriding the `setup` method in the `Backend` instance with a mechanism where you provide Snabbdom's JavaScript artifacts yourself.

