# Isreal Swan

Snowman as a service

> If you build me a snowman, then I'll build one for you.

This service is hosted at https://isreal.shpadoinkle.org, and allows you to post
Shpadoinkle code, and see the resulting UI in your browser. For example lets say we have
the following file locally:

`Hello.hs` containing

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

You can send this to Isreal Swan with the following `curl` command.

```bash
curl -X POST -H "Content-Type:application/octet-stream" --data-binary @Hello.hs \
  https://isreal.shpadoinkle.org/compile/hello-token
```

Notice **hello-token** in the URL. It's _on you_ to make this a unique token for your work,
as the system is open ended. Reusing this token will result in incremental rebuilds, which
are much faster. Also please note, these spaces are ephemeral and will be deleted.

The `curl` command will respond with `Either` an error message from the compiler, or
a message indicating success. If your code compiled successfully, you will be able to see
the resulting UI here:

`https://isreal.shpadoinkle.org/serve/hello-token/index.html`


## Deps & Such

Many common haskell dependencies are provided.
The environment is based on the cabal file located [here](https://gitlab.com/fresheyeball/Shpadoinkle/-/blob/master/isreal/swan/swan.cabal).

## Clean Up

Clean up your token

```bash
curl -X DELETE http://localhost:8080/clean/hello-token
```
