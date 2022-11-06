{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
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
import           Control.Monad                (unless, void)
import           Control.Monad.IO.Class       (liftIO)
import           Data.Text
import           Shpadoinkle.JSFFI            (JSObject, JSString, JSVal,
                                               getProp, global, jsTo, mkFun',
                                               toBoolLax, toIntLax, toTextLax,
                                               (#), (#-))
import           UnliftIO.Concurrent          (forkIO)
import           UnliftIO.STM

import           Shpadoinkle
import           Shpadoinkle.Html.Event.Basic
import           Shpadoinkle.Html.TH
import           Shpadoinkle.Keyboard


toKeyCodeLax :: JSVal -> JSM KeyCode
toKeyCodeLax = fmap fromIntegral . toIntLax


mkWithFormVal ::  (JSVal -> JSM v) -> Text -> JSString -> (v -> Continuation m a) -> (Text, Prop m a)
mkWithFormVal valTo evt from f = listenRaw evt $ \(RawNode n) _ ->
  f <$> liftJSM (valTo =<< getProp from n)


onInputC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onInputC = mkWithFormVal toTextLax "input" "value"
$(mkEventVariantsAfforded "input" ''Text)


onBeforeinputC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onBeforeinputC = mkWithFormVal toTextLax "beforeinput" "value"
$(mkEventVariantsAfforded "beforeinput" ''Text)


onOptionC ::  (Text -> Continuation m a) -> (Text, Prop m a)
onOptionC = mkWithFormVal toTextLax "change" "value"
$(mkEventVariantsAfforded "option" ''Text)


mkOnKey ::  Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkOnKey t f = listenRaw t $ \_ (RawEvent e) ->
  f <$> liftJSM (toKeyCodeLax =<< getProp ("keyCode" :: Text) e)


onKeyupC, onKeydownC, onKeypressC :: (KeyCode -> Continuation m a) -> (Text, Prop m a)
onKeyupC    = mkOnKey "keyup"
onKeydownC  = mkOnKey "keydown"
onKeypressC = mkOnKey "keypress"
$(mkEventVariantsAfforded "keyup"    ''KeyCode)
$(mkEventVariantsAfforded "keydown"  ''KeyCode)
$(mkEventVariantsAfforded "keypress" ''KeyCode)


onCheckC ::  (Bool -> Continuation m a) -> (Text, Prop m a)
onCheckC = mkWithFormVal toBoolLax "change" "checked"
$(mkEventVariantsAfforded "check" ''Bool)


preventDefault :: RawEvent -> JSM ()
preventDefault e = unRawEvent e #- ("preventDefault" :: String) $ ([] :: [()])


stopPropagation :: RawEvent -> JSM ()
stopPropagation e = unRawEvent e #- ("stopPropagation" :: String) $ ([] :: [()])


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

     (global #- ("addEventListener" :: Text)) . ("click" :: Text,) =<<
        mkFun' (\case
          evt:_ -> void . forkIO $ do

            target :: JSVal <- jsTo @JSObject evt >>= getProp ("target" :: Text)
            onTarget <- elm # ("contains" :: Text) $ target
            toBoolLax onTarget >>= \case
              False -> notify
              _     -> return ()

          [] -> pure ())

     return stream
  )
$(mkEventVariants "clickAway")


mkGlobalKey :: Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkGlobalKey evtName c =
  ( "global" <> evtName
  , PPotato $ \_ -> liftJSM $ do

     (notify, stream) <- mkGlobalMailboxAfforded c

     (global #- ("addEventListener" :: Text)) . (evtName,) =<<
        mkFun' (\case
           e:_ -> notify =<< toKeyCodeLax =<< getProp ("keyCode" :: Text) =<< jsTo @JSObject e
           []  -> return ())

     return stream
  )


mkGlobalKeyNoRepeat :: Text -> (KeyCode -> Continuation m a) -> (Text, Prop m a)
mkGlobalKeyNoRepeat evtName c =
  ( "global" <> evtName
  , PPotato $ \_ -> liftJSM $ do

     (notify, stream) <- mkGlobalMailboxAfforded c

     (global #- ("addEventListener" :: Text)) . (evtName,) =<<
        mkFun' (\case
           e:_ -> do
             eObj <- jsTo @JSObject e
             isRepeat <- toBoolLax =<< getProp ("repeat" :: Text) eObj
             unless isRepeat $
               notify =<< toKeyCodeLax =<< getProp ("keyCode" :: Text) eObj
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


onGlobalKeyDownNoRepeatC :: (KeyCode -> Continuation m a) -> (Text, Prop m a)
onGlobalKeyDownNoRepeatC  = mkGlobalKeyNoRepeat "keydown"
$(mkEventVariantsAfforded "globalKeyDownNoRepeat"  ''KeyCode)


onEscapeC :: Continuation m a -> (Text, Prop m a)
onEscapeC c = onKeyupC $ \case 27 -> c; _ -> done
$(mkEventVariants "escape")


onEnterC :: (Text -> Continuation m a) -> (Text, Prop m a)
onEnterC f = listenRaw "keyup" $ \(RawNode n) _ -> liftJSM $
  f <$> (toTextLax =<< getProp ("value" :: Text) =<< jsTo @JSObject n)
$(mkEventVariantsAfforded "enter" ''Text)

