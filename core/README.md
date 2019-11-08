# Shpadoinkle Core

Shpadoinkle is a programming model for UI development, oriented around simplicity,
performance, and ergonomics.

## The core concept

Is to model the user interface as a pure function from some model `a` to Html.
This is not a new idea in the slightest. Declaratively describing the view in terms
of the model through a data structure is the dominant approach in UI today. And
for good reason.

If all we need is to render something based on some `a` we can have `Html` be a
simple data structure where `Html :: Type`.

```haskell
view :: a -> Html
```

This might look something like.

```haskell
view :: Text -> Html
view username =
  H "div" [ ("class", "greeting") ] [ Text $ "Hi there!" <> username ]
```

## Events and effects

Which is all well and good, and something we might expect from a static renderer,
like Heist, or Blaze. Shpadoinkle handles this by allowing for `Html` to have two
type variables associated with events, `Html :: (Type -> Type) -> Type -> Type`.

The first is typically some Monad you wish to use in response to events `m`, and
the second is the payload of those events, typically the model for your view `a`.

These variables in `Html m a` are strickly about event listeners, so any view
that doesn't have event listeners should be parametic in both `m` and `a`.

Let's look at a toggle as an example.

```haskell
toggle :: Applicative m => Bool -> Html m Bool
toggle b = h "div" []
  [ text $ "Currently it's " <> if b then "ON" else "OFF"
  , h "button" [ listen "click" (not b) ] [ text "Toggle" ]
  ]
```

That's it, we have a stateful view. When the user click's on
the "Toggle" button the state will switch. Because we do a pure
state transition in this function, `m` need only be `Applicative`.
We could put `Identity` here if we wanted to, but keeping `m` general
helps our views compose.

But what if we need to do _more_? Well we can update our `m` to
have more functionality. Let's add some logging to the console.

```haskell
toggle :: Bool -> Html IO Bool
toggle b = h "div" []
  [ text $ "Currently it's " <> if b then "ON" else "OFF"
  , h "button"
    [ listen' "click" $ do
        putStrLn "We toggled!"
        return $ not b
    ] [ text "Toggle" ]
  ]
```

What if we want to access some record of capabilities? Or update some
concurrent memory thing? Let's say we have an enterprise grade Monad,

```haskell
newtype App a = App { runApp :: RIO (TVar Metrics) a }
  deriving (Functor, Applicative, Monad, MonadReader (TVar Metrics), MonadIO, MonadJSM)


toggle :: Bool -> Html App Bool
toggle b = h "div" []
  [ text $ "Currently it's " <> if b then "ON" else "OFF"
  , h "button"
    [ listen' "click" $ do
        metrics <- ask
        liftIO $ do
          atomically . modifyTVar metrics $
            \m -> m { toggleCount = toggleCount m + 1 }
          putStrLn "We toggled!"
        return $ not b
    ] [ text "Toggle" ]
  ]
```

## Composing views

In Shpadoinkle we can compose views without impedance if the types match,
or are parametric. For example.

```haskell
hero :: Html m a
hero = h "h1" [] [ text "Online String Reverse" ]

input :: Html m Text
input = h "input" [ onInput id ] []

view :: Text -> Html m Text
view s = h "div" []
  [ hero -- no impedance, this Html is fully generic
  , input -- no impedance, this Html has matching types `(Text ~ Text)`
  , text $ "Reversed: \"" <> reverse s <> "\""
  ]
```

If you have nesting, with different types,
we can resolve the mismatch using 'fmap' like so:

```haskell
input :: Html m Text
input = h "input" [ onInput id ] []

view :: (Int, Text) -> Html m (Int, Text)
view (i,t) = h "div" []

  -- here we update the `Text` side of the model
  -- with the value produced by `input`, and we
  -- increment the `Int` as well.
  [ (\t_ -> (i + 1, t_)) <$> input
  , text $ "Reversed: \"" <> reverse t <> "\""
  , text $ "you have reversed " <> pack (show i) <> " strings"
  ]
```

## The primative

The Shpadoinkle programming model core primative is the `shpadoinkle` function.

```haskell
shpadoinkle
  :: (Shpadoinkle b m a, Territory t, Eq a) =>
  => (m ~> JSM) -> (t a -> b m ~> m) -- How to render
  -> a -> t a                        -- What is our model
  -> (a -> Html (b m) a)             -- What to render
  -> b m RawNode -> JSM ()           -- Actually render
```

This is the machine that runs a Shpadoinkle view. To run we need
the following ingredients.

### `m ~> JSM`

We need a _Natural Transformation_ from our `m` to `JSM`, so that
we can perform the needed JavaScript effects in JSM from the `m`
you provide.

### `t a -> b m ~> m`

This a function that takes a state container of some kind `t`,
and returns a _Natural Transformation_ from our Shpadoinkle backend `b`,
to our monad `m`. Backends kind of works like Monad Transformers, where
`b` wraps our Monad `m`, and needs to be unwrappable.

### `a`

This is the initial value of our model. This will be passed to our view
for the first render.

### `t a`

This is the state container `t` that will drive the view. When the state
changes, we should re-render the view. The semantic behind determing when
to do this, is upto you via the `Territory` type class. Typically this is
just a `TVar` as that is the provided cannonical implimentation.

### `a -> Html (b m) a`

This is the view function, you actual application to render. It takes
the model and returns the html to render, such that it's events produce the
same model.

### `b m RawNode`

This is the raw node we that will wrap our view. If you want the Shpadoinkle view
to be the entire page, then you want to pass `document.body` as this node.
You could use this to embed a Shpadoinkle application into another application,
(such a Reflex-dom or Miso).

