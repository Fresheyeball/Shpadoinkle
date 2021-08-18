{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


-- | This module provides a DSL of Events found on HTML elements.
-- This DSL is entirely optional. You may use the 'Prop's 'PListener' constructor
-- provided by Shpadoinkle core and completely ignore this module.
-- You can use the 'listener', 'listen', 'listenRaw', 'listenC', and 'listenM' convenience
-- functions as well without using this module. For those who like a typed
-- DSL with named functions and overloading, this is for you.
--
-- All listeners come in 4 flavors. Unctuous flavors. Plain ('onInput'), continuous ('onInputC'), monadic ('onInputM'), and forgetful ('onInputM_').
--
-- A flavor providing access to the 'RawNode' and the 'RawEvent' are not provided
-- here. If you want access to these, try the 'listenRaw' constructor. The intent
-- of this DSL is to provide simple named functions.
--
-- Right now this module features limited specialization, but ideally we specialize
-- all of these listeners. For example, the 'onInput' listener takes a function
-- @(Text -> a -> a)@ where 'Text' is the current value of the input and 'onKeyup' takes
-- a function of type @(KeyCode -> a -> a)@ from 'Shpadoinkle.Keyboard'. Mouse move
-- listeners, for example, should take a function of @((Float, Float) -> a -> a)@, but
-- this work is not yet done.


module Shpadoinkle.Html.Event
  ( module Shpadoinkle.Html.Event
  , module Shpadoinkle.Html.Event.Basic
  ) where


import           Control.Concurrent.STM       (retry)
import           Control.Lens                 ((^.))
import           Control.Monad                (void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Text
import           GHCJS.DOM.Types              hiding (Text)
import           Language.Javascript.JSaddle  hiding (JSM, liftJSM, toJSString)
import           UnliftIO.Concurrent          (forkIO)
import           UnliftIO.STM

import           Shpadoinkle
import           Shpadoinkle.Html.Event.Basic
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


mkWithFormVal ::  (JSVal -> JSM v) -> Text -> JSString -> (v -> Continuation m a) -> (Text, Prop m a)
mkWithFormVal valTo evt from f = listenRaw evt $ \(RawNode n) _ ->
  f <$> liftJSM (valTo =<< unsafeGetProp from =<< valToObject n)


onInputC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onInputC = mkWithFormVal valToText "input" "value"
$(mkEventVariantsAfforded "input" ''Text)


onBeforeinputC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onBeforeinputC = mkWithFormVal valToText "beforeinput" "value"
$(mkEventVariantsAfforded "beforeinput" ''Text)


onOptionC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onOptionC = mkWithFormVal valToText "change" "value"
$(mkEventVariantsAfforded "option" ''Text)


mkOnKey ::  Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  f <$> liftJSM (fmap round $ valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e)


onKeyupC, onKeydownC, onKeypressC :: (KeyCode -> Continuation m a) -> (Text, Prop m a)
onKeyupC    = mkOnKey "keyup"
onKeydownC  = mkOnKey "keydown"
onKeypressC = mkOnKey "keypress"
$(mkEventVariantsAfforded "keyup"    ''KeyCode)
$(mkEventVariantsAfforded "keydown"  ''KeyCode)
$(mkEventVariantsAfforded "keypress" ''KeyCode)


onCheckC ::  (Bool -> Continuation m a) -> (Text, Prop m a)
onCheckC = mkWithFormVal valToBool "change" "checked"
$(mkEventVariantsAfforded "check" ''Bool)


preventDefault :: RawEvent -> JSM ()
preventDefault e = void $ valToObject e # ("preventDefault" :: String) $ ([] :: [()])


onSubmitC :: Continuation m a -> (Text, Prop m a)
onSubmitC m = listenRaw "submit" $ \_ e -> preventDefault e >> return m
$(mkEventVariants "submit")


mkGlobalMailbox :: Continuation m a -> JSM (JSM (), STM (Continuation m a))
mkGlobalMailbox c = do
  (notify, stream) <- mkGlobalMailboxAfforded (const c)
  return (notify (), stream)


mkGlobalMailboxAfforded :: (b -> Continuation m a) -> JSM (b -> JSM (), STM (Continuation m a))
mkGlobalMailboxAfforded bc = do
  (notify, twas) <- liftIO $ (,) <$> newTVarIO (0, Nothing) <*> newTVarIO (0 :: Int)
  return (\b -> atomically $ modifyTVar notify (\(i, _) -> (i + 1, Just b)), do
    (new', b) <- readTVar notify
    old  <- readTVar twas
    case b of
      Just b' | new' /= old -> bc b' <$ writeTVar twas new'
      _                     -> retry)


onClickAwayC :: Continuation m a -> (Text, Prop m a)
onClickAwayC c =
  ( "onclickaway"
  , PPotato $ \(RawNode elm) -> liftJSM $ do

     (notify, stream) <- mkGlobalMailbox c

     void $ jsg ("document" :: Text) ^. js2 ("addEventListener" :: Text) ("click" :: Text)
        (fun $ \_ _ -> \case
          evt:_ -> void . forkIO $ do

            target   <- evt ^. js ("target" :: Text)
            onTarget <- fromJSVal =<< elm ^. js1 ("contains" :: Text) target
            case onTarget of
              Just False -> notify
              _          -> return ()

          [] -> pure ())

     return stream
  )
$(mkEventVariants "clickAway")


mkGlobalKey :: Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkGlobalKey evtName c =
  ( "global" <> evtName
  , PPotato $ \_ -> liftJSM $ do

     (notify, stream) <- mkGlobalMailboxAfforded c

     void $ jsg ("window" :: Text) ^. js2 ("addEventListener" :: Text) evtName
        (fun $ \_ _ -> \case
           e:_ -> notify . round =<< valToNumber =<< unsafeGetProp "keyCode" =<< valToObject e
           []  -> return ())

     return stream
  )


onGlobalKeyPressC, onGlobalKeyDownC, onGlobalKeyUpC :: (KeyCode -> Continuation m a) -> (Text, Prop m a)
onGlobalKeyPressC = mkGlobalKey "keypress"
onGlobalKeyDownC  = mkGlobalKey "keydown"
onGlobalKeyUpC    = mkGlobalKey "keyup"
$(mkEventVariantsAfforded "globalKeyPress" ''KeyCode)
$(mkEventVariantsAfforded "globalKeyDown"  ''KeyCode)
$(mkEventVariantsAfforded "globalKeyUp"    ''KeyCode)


onEscapeC :: Continuation m a -> (Text, Prop m a)
onEscapeC c = onKeyupC $ \case 27 -> c; _ -> done
$(mkEventVariants "escape")


onEnterC :: (Text -> Continuation m a) -> (Text, Prop m a)
onEnterC f = listenRaw "keyup" $ \(RawNode n) _ -> liftJSM $
  f <$> (valToText =<< unsafeGetProp "value"
                   =<< valToObject n)
$(mkEventVariantsAfforded "enter" ''Text)

