{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE BangPatterns           #-}
{-# LANGUAGE CPP                    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE ExplicitNamespaces     #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TupleSections          #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE UndecidableInstances   #-}

{-|
   I think I know precisely what I mean.

   A frontend abstraction motivated by simplicity, performance, and egonomics.
   This module provides core abstractions and types with almost no implimentation details. IE no batteries included.
   You may use this model a la carte, build ontop of it, or include more Backend packages for additional batteries.

   Backend is focused on letting you build your frontend the way you want to. And so is as unopinionated as possible, beyond providing a concrete programming model.
-}

module Shpadoinkle
  ( Html (..), Prop (..), Props
  , mapHtml, mapProp, mapProps, mapChildren
  , Backend (..)
  , shpadoinkle, fullPage, fullPageJSM
  , Territory (..)
  , type (~>), Html'
  , RawNode (..), RawEvent (..)
  , h, text, flag, textProp
  , listener, listen, listenRaw, listen'
  , baked
  , props, children, name, textContent, injectProps
  , MonadJSM, JSM, liftJSM
  , newTVarIO, readTVarIO
  , runJSorWarp
  , runJSM, askJSM
  ) where


import           Data.Kind
import           Data.String
import           Data.Text
import           GHC.Conc                         (retry)
import           Language.Javascript.JSaddle
#ifndef ghcjs_HOST_OS
import           Language.Javascript.JSaddle.Warp
#endif
import           UnliftIO.Concurrent
import           UnliftIO.STM


-- | This is the core type in Backend.
-- The (Html m) 'Functor' is used to describe Html documents.
-- Please note, this is NOT a the Virtual Dom used by Backend
-- this type backs a DSL that is then /interpreted/ into Virual Dom
-- by the backend of your choosing. Html comments are not supported.
data Html :: (Type -> Type) -> Type -> Type where
  -- | A standard node in the dom tree
  Node :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
  -- | If you can bake an element into a 'RawNode' you can embed it as a baked potato.
  -- Backend does not provide any state management or abstraction to deal with
  -- custom embeded content. It's own you to decide how and when this 'RawNode' will
  -- be updated. For example, if you wanted to embed a google map as a baked potato,
  -- and you are driving your Backend view with a 'TVar', you would need to build
  -- the 'RawNode' for this map /outside/ of your Backend view, and pass it in
  -- as an argument. The 'RawNode' is a reference you control.
  Potato :: JSM RawNode -> Html m o
  -- | The humble text node
  TextNode :: Text -> Html m o


-- | Natural Transformation
type m ~> n = forall a. m a -> n a

-- | A type alias to support scenarios where
-- the view code event listeners are pure.
type Html' a = forall m. Applicative m => Html m a


-- | If you can provide a Natural Transformation from one Monad to another
-- you may change the action of @Html@
mapHtml :: Functor m => (m ~> n) -> Html m o -> Html n o
mapHtml f = \case
  Node t ps cs -> Node t (fmap (mapProp f) <$> ps) (mapHtml f <$> cs)
  Potato p -> Potato p
  TextNode t -> TextNode t


-- | If you can provide a Natural Transformation from one Monad to another
-- you may change the action of @Prop@
mapProp :: (m ~> n) -> Prop m o -> Prop n o
mapProp f = \case
  PListener g -> PListener (\x y -> f (g x y))
  PText t     -> PText t
  PFlag b     -> PFlag b


-- | Transform the properites of some Node. This has no effect on @TextNode@s or @Potato@s
mapProps :: ([(Text, Prop m o)] -> [(Text, Prop m o)]) -> Html m o -> Html m o
mapProps f = \case
  Node t ps cs -> Node t (f ps) cs
  t -> t


-- | Transform the children of some Node. This has no effect on @TextNode@s or @Potato@s
mapChildren :: ([Html m a] -> [Html m a]) -> Html m a -> Html m a
mapChildren f = \case
  Node t ps cs -> Node t ps (f cs)
  t -> t


-- | Lens to props
props :: Applicative f => ([(Text, Prop m a)] -> f [(Text, Prop m a)]) -> Html m a -> f (Html m a)
props inj = \case
  Node t ps cs -> (\ps' -> Node t ps' cs) <$> inj ps
  t -> pure t


-- | Lens to children
children :: Applicative f => ([Html m a] -> f [Html m a]) -> Html m a -> f (Html m a)
children inj = \case
  Node t ps cs -> Node t ps <$> inj cs
  t -> pure t


-- | Lens to tag name
name :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
name inj = \case
  Node t ps cs -> (\t' -> Node t' ps cs) <$> inj t
  t -> pure t


-- | Lens to content of @TextNode@s
textContent :: Applicative f => (Text -> f Text) -> Html m a -> f (Html m a)
textContent inj = \case
  TextNode t -> TextNode <$> inj t
  n -> pure n


-- | Inject props into an existing @Node@
injectProps :: [(Text, Prop m o)] -> Html m o -> Html m o
injectProps ps html = case html of
  Node t ps' cs -> Node t (ps' ++ ps) cs
  x             -> x


-- | JSX style @h@ constructor
h :: Text -> [(Text, Prop m o)] -> [Html m o] -> Html m o
h = Node
{-# INLINE h #-}


-- | Construct a 'Potato' from a 'JSM' action producing a 'RawNode'
baked :: JSM RawNode -> Html m o
baked = Potato
{-# INLINE baked #-}


-- | Construct a 'TextNode'
text :: Text -> Html m o
text = TextNode
{-# INLINE text #-}


-- | Construct a 'PFlag'
flag :: Bool -> Prop m o
flag = PFlag
{-# INLINE flag #-}


-- | Construct a `PText`
textProp :: Text -> Prop m o
textProp = PText
{-# INLINE textProp #-}


-- | Construct a simple 'PListener` that will perform an action.
listener :: m o -> Prop m o
listener = PListener . const . const
{-# INLINE listener #-}


-- | Construct a 'PListener' from it's 'Text' name a raw listener.
listenRaw :: Text -> (RawNode -> RawEvent -> m o) -> (Text, Prop m o)
listenRaw k = (,) k . PListener
{-# INLINE listenRaw #-}


-- | Construct a 'PListener' from it's 'Text' name and a Monad action.
listen :: Text -> m o -> (Text, Prop m o)
listen k = listenRaw k . const . const
{-# INLINE listen #-}


-- | Construct a 'PListener' from it's 'Text' name and an ouput value.
listen' :: Applicative m => Text -> o -> (Text, Prop m o)
listen' k f = listen k $ pure f


-- | @(Html m)@ is not a 'Monad', and not even 'Applicative', by design.
deriving instance Functor m => Functor (Html m)


-- | Properties of a Dom node, Backend does not use attributes directly,
-- but rather is focued on the more capable properties that may be set on a dom
-- node in JavaScript. If you wish to add attributes, you may do so
-- by setting its corrosponding property.
data Prop m o where
  -- | A text property
  PText :: Text -> Prop m o
  -- | Event listeners are provided with the 'RawNode' target, and the 'RawEvent', and may perform
  -- a Monadic action such as a side effect. This is the one and only place where you may
  -- introduce a custom Monadic action.
  PListener :: (RawNode -> RawEvent -> m o) -> Prop m o
  -- | A boolean property, works as a flag
  -- for example @("disabled", PFlag False)@ has no effect
  -- while @("disabled", PFlag True)@ will add the @disabled@ attribute
  PFlag :: Bool -> Prop m o


-- | Props are also merely 'Functor's not 'Monad's and not 'Applicative' by design.
deriving instance Functor m => Functor (Prop m)

-- | Type alias for convenience. Typing out the nested brackets is tiresome.
type Props m o = [(Text, Prop m o)]


-- | Strings are overload to html text nodes
-- @
--   "hiya" = TextNode "hiya"
-- @
instance IsString (Html m o) where
  fromString = TextNode . pack
  {-# INLINE fromString #-}


-- | Strings are overload as text props
-- @
--   ("id", "foo") = ("id", PText "foo")
-- @
instance IsString (Prop m o) where
  fromString = PText . pack
  {-# INLINE fromString #-}

-- | Strings are overload as the class property
-- @
--   "active" = ("className", PText "active")
-- @
instance {-# OVERLAPPING #-} IsString [(Text, Prop m o)] where
  fromString = pure . ("className", ) . PText . pack
  {-# INLINE fromString #-}


-- | A dom node reference.
-- Useful for building baked potatoes, and binding a Backend view to the page
newtype RawNode  = RawNode  { unRawNode  :: JSVal }
-- | A raw event object reference
newtype RawEvent = RawEvent { unRawEvent :: JSVal }
instance ToJSVal   RawNode where toJSVal   = return . unRawNode
instance FromJSVal RawNode where fromJSVal = return . Just . RawNode


-- |
-- patch raw Nothing >=> patch raw Nothing = patch raw Nothing

-- | The Backend class describes a backend that can render 'Html'.
-- Backends are generally Monad Transformers @b@ over some Monad @m@.
class Backend b m a | b m -> a where
  -- | VNode type family allows backends to have their own Virtual Dom.
  -- As such we can change out the rendering of our Backend view
  -- with new backends without updating our view logic.
  type VNode b m
  -- | A backend must be able to interpret 'Html' into its own internal Virtual Dom
  interpret
    :: (m ~> JSM)
    -- ^ Natural transformation for some @m@ to 'JSM'.
    -- This is how Backend get access to 'JSM' to perform the rendering side effect
    -> Html (b m) a
    -- ^ 'Html' to interpret
    -> b m (VNode b m)
    -- ^ Effect producing the Virtual Dom representation

  -- | A backend must be able to patch the 'RawNode' containing the view, with a
  -- new view if the Virtual Dom changed.
  patch
    :: RawNode
    -- ^ The container for rendering the Backend view.
    -> Maybe (VNode b m)
    -- ^ Perhaps there is a previous Virtual Dom for use to diff. Will be 'Nothing' on the first run.
    -> VNode b m
    -- ^ New Virtual Dom to render.
    -> b m (VNode b m)
    -- ^ Effect producing and updated virtual dom. This is not needed by all backends.
    -- Some JavaScript based backends need to do this for the next tick. Regardless whatever
    -- 'VNode' the effect produces will be passed as the previous Virtual Dom on the next render.

  -- | A backend may perform some inperative setup steps
  setup :: JSM () -> b m ()


-- | Shpadoinkling requires a Territory, such as Colorado Territory.
-- This class provides for the state container. As such you may use any
-- type you wish where this semantic can be implimented.
class Territory s where
  -- | How do we update the state?
  writeUpdate :: s a -> (a -> JSM a) -> JSM ()
  -- | When should consider a state updated? This is akin to React's component should update thing.
  -- The idea is to provide a semantic for when we consider the model to have changed.
  shouldUpdate :: Eq a => (b -> a -> JSM b) -> b -> s a -> JSM ()
  -- | Create a new territory
  createTerritory :: a -> JSM (s a)


-- | Cannoncal default implimentation of 'Territory' is just a 'TVar'.
-- However there is nothing stopping your from writing your own alternative
-- for a @Dynamic t@ from Reflex Dom, or some JavaScript based container.
instance Territory TVar where
  writeUpdate x f = do
    a <- f =<< readTVarIO x
    atomically $ writeTVar x a
  {-# INLINE writeUpdate #-}

  shouldUpdate sun prev model = do
    i' <- readTVarIO model
    p  <- createTerritory i'
    () <$ forkIO (go prev p)
    where
      go x p = do
        a <- atomically $ do
          new' <- readTVar model
          old  <- readTVar p
          if new' == old then retry else new' <$ writeTVar p new'
        y <- sun x a
        go y p

  createTerritory = newTVarIO
  {-# INLINE createTerritory #-}


-- | The core view instantiation function.
-- This combines a backend, a territory, and a model
-- and renders the Backend view to the page.
shpadoinkle
  :: forall b m a t
   . Backend b m a => Territory t => Eq a
  => (m ~> JSM)
  -- ^ how to be get to JSM?
  -> (t a -> b m ~> m)
  -- ^ What backend are we running?
  -> a
  -- ^ what is the initial state?
  -> t a
  -- ^ how can we know when to update?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> JSM ()
shpadoinkle toJSM toM initial model view stage = do
  let
    j :: b m ~> JSM
    j = toJSM . toM model

    go :: RawNode -> VNode b m -> a -> JSM (VNode b m)
    go c n a = do
      !m  <- j $ interpret toJSM (view a)
      j $ patch c (Just n) m

  j . setup $ do
    c <- j stage
    n <- j $ interpret toJSM (view initial)
    _ <- shouldUpdate (go c) n model
    _ <- j $ patch c Nothing n :: JSM (VNode b m)
    return ()

-- | Wrapper around @shpadoinkle@ for full page apps
-- that do not need outside control of the territory
fullPage
  :: Backend b m a => Territory t => Eq a
  => (m ~> JSM)
  -- ^ how do we get to JSM?
  -> (t a -> b m ~> m)
  -- ^ What backend are we running?
  -> a
  -- ^ what is the initial state?
  -> (a -> Html (b m) a)
  -- ^ how should the html look?
  -> b m RawNode
  -- ^ where do we render?
  -> JSM ()
fullPage g f i view getStage = do
  model <- createTerritory i
  shpadoinkle g f i model view getStage
{-# INLINE fullPage #-}


-- | Wrapper around @shpadoinkle@ for full page apps
-- that do not need outside control of the territory
-- where actions are performed directly in JSM.
--
-- This set of assumptions is extremely common when starting
-- a new project.
fullPageJSM
  :: Backend b JSM a => Territory t => Eq a
  => (t a -> b JSM ~> JSM)
  -- ^ What backend are we running?
  -> a
  -- ^ what is the initial state?
  -> (a -> Html (b JSM) a)
  -- ^ how should the html look?
  -> b JSM RawNode
  -- ^ where do we render?
  -> JSM ()
fullPageJSM = fullPage id
{-# INLINE fullPageJSM #-}


-- | Start the program!
--
-- For GHC or GHCjs. I saved your from using CPP directly. Your welcome.
runJSorWarp :: Int -> JSM () -> IO ()
#ifdef ghcjs_HOST_OS
runJSorWarp _ = id
{-# INLINE runJSorWarp #-}
#else
runJSorWarp = run
{-# INLINE runJSorWarp #-}
#endif
