# Shpadoinkle Backend ParDiff

[![Goldwater](https://gitlab.com/fresheyeball/Shpadoinkle/badges/master/pipeline.svg)](https://gitlab.com/fresheyeball/Shpadoinkle)
[![BSD-3](https://img.shields.io/badge/License-BSD%203--Clause-blue.svg)](https://opensource.org/licenses/BSD-3-Clause)
[![built with nix](https://img.shields.io/badge/built%20with-nix-41439a)](https://builtwithnix.org)
[![Hackage](https://img.shields.io/hackage/v/Shpadoinkle-backend-pardiff.svg)](https://hackage.haskell.org/package/Shpadoinkle-backend-pardiff)
[![Hackage Deps](https://img.shields.io/hackage-deps/v/Shpadoinkle-backend-pardiff.svg)](http://packdeps.haskellers.com/reverse/Shpadoinkle-backend-pardiff)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/Shpadoinkle-backend-pardiff/badge)](https://matrix.hackage.haskell.org/#/package/Shpadoinkle-backend-pardiff)


Shpadoinkle's ParDiff backend is a virtual dom diffing system written in pure Haskell.
It currently serves as the canonical backend for Shpadoinkle, such that the behavior of
other backends should conform.

The virtual tree in ParDiff contains a reference to the `RawNode` for each element. Merging
unkeyed and keyed virtual dom techniques together. This allows for rendering to be performed
in a keyed fashion for all nodes, while not requiring additional memory or developer overhead.

The diffing itself is a lawful usage of `alignWith` from the `Data.These` package. By modeling
Html as an Alignable Functor, you get principled diffing with clear separation of concerns.

IO is done using JSaddle, and works with both GHC and GHCjs.
