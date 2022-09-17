{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# OPTIONS_GHC -fno-warn-type-defaults              #-}


module Shpadoinkle.Website.Page.FourOhFour where


import           Control.Concurrent.STM                  (TVar, atomically,
                                                          modifyTVar, retry)
import           Control.Monad.IO.Class                  (liftIO)
import           Data.Text                               (Text)
import           GHC.Generics                            (Generic)
import           GHCJS.DOM                               (currentDocumentUnchecked,
                                                          currentWindowUnchecked)
import           GHCJS.DOM.Document                      (createElement)
import           GHCJS.DOM.Element                       (setId)
import           GHCJS.DOM.NonElementParentNode          (getElementById)
import           GHCJS.DOM.RequestAnimationFrameCallback (RequestAnimationFrameCallback,
                                                          newRequestAnimationFrameCallback)
import           GHCJS.DOM.Window                        (Window,
                                                          requestAnimationFrame)
import           Shpadoinkle                             (JSM, NFData,
                                                          RawNode (..),
                                                          newTVarIO,
                                                          shpadoinkle)
import           Shpadoinkle.Backend.Snabbdom            (runSnabbdom)
import           Shpadoinkle.Html                        as H
import           Shpadoinkle.Html.TH.AssetLink           (assetLink)
import           Shpadoinkle.JSFFI                       (toJSVal)
import           Shpadoinkle.Keyboard                    (pattern Ctrl,
                                                          pattern LeftArrow,
                                                          pattern RightArrow)
import           UnliftIO.Concurrent                     (forkIO, threadDelay)


default (Text)


data Direction = FaceLeft | FaceRight
  deriving (Eq, Ord, Show, Generic, NFData)


data State = Idle | Walking | Shooting Clock
  deriving (Eq, Ord, Show, Generic, NFData)


newtype Position = Position { unPosition :: Float }
  deriving stock Generic
  deriving newtype (Eq, Ord, Num, Show) deriving anyclass (NFData)


newtype Clock = Clock { unClock :: Double }
  deriving stock Generic
  deriving newtype (Eq, Ord, Num, Show) deriving anyclass (NFData)


data Game = Game
  { position  :: Position
  , clock     :: Clock
  , state     :: State
  , direction :: Direction
  } deriving (Eq, Ord, Show, Generic, NFData)


spriteDim :: Int
spriteDim = 48


game :: Game -> Html m Game
game g = H.div [id' "game"] . pure $ H.div'
  [ id' "avatar"
  , styleProp styles
  , onGlobalKeyDownNoRepeat $ \case
     LeftArrow  -> \g' -> g' { state = Walking, direction = FaceLeft }
     RightArrow -> \g' -> g' { state = Walking, direction = FaceRight }
     Ctrl       -> \g' -> g' { state = Shooting (clock g') }
     _          -> id
  , onGlobalKeyUp $ \case
     LeftArrow  -> \g' -> g' { state = Idle }
     RightArrow -> \g' -> g' { state = Idle }
     _          -> id
  ]
  where
  styles =
    [ ("height", px spriteDim)
    , ("width", px spriteDim)
    , ("background-image", "url(" <> spriteImage <> ")")
    , ("background-position", px (spriteDim * (spriteCount - spriteTime)) <> ", 0")
    , ("position", "absolute")
    , ("bottom", "0")
    , ("transform",
                  "translate3d(" <> px (position g) <> ",0,0) "
    <> "scaleX(" <> (case direction g of FaceLeft -> "-1"; FaceRight -> "1") <> ")"
      )
    ]
  spriteImage = toSpriteImage $ state g
  spriteCount = toSpriteCount $ state g
  spriteTime  = toSpriteTime (clock g) (state g)


fps :: Num n => n
fps = 12


toSpriteTime :: Clock -> State -> Int
toSpriteTime c gs = floor (unClock since / (1000 / fps)) `mod` toSpriteCount gs
  where since = case gs of Shooting c' -> c - c'; _ -> c


toSpriteImage :: State -> Text
toSpriteImage = \case
  Shooting _ -> $(assetLink "/assets/game/CowBoyShoot.png")
  Walking    -> $(assetLink "/assets/game/CowBoyWalking.png")
  Idle       -> $(assetLink "/assets/game/CowBoyIdle.png")


toSpriteCount :: State -> Int
toSpriteCount = \case
  Shooting _ -> 5
  Walking    -> 8
  Idle       -> 8


tick :: Clock -> Game -> Game
tick d g = g
  { clock = d
  , position = if state g /= Walking then position g else
    let moveBy = Position . realToFrac $ 100 * (unClock (d - clock g) / 1000) in case direction g of
      FaceLeft  -> position g - moveBy
      FaceRight -> position g + moveBy
  , state = case state g of
      Shooting _ | toSpriteTime d (state g) == toSpriteCount (state g) - 1 -> Idle
      _ -> state g
  }


animate :: Window -> TVar Game -> JSM RequestAnimationFrameCallback
animate win model = newRequestAnimationFrameCallback $ \d -> () <$ do
  liftIO . atomically . modifyTVar model . tick $ Clock d
  requestAnimationFrame win =<< animate win model


play :: JSM RawNode
play = do
  let gameId = "game"
  doc <- currentDocumentUnchecked
  isSubsequent <- traverse toJSVal =<< getElementById doc gameId
  case isSubsequent of
    Just raw -> return $ RawNode raw
    Nothing -> do
      win <- currentWindowUnchecked
      elm <- createElement doc "div"
      setId elm gameId
      model <- newTVarIO $ Game 24 0 Idle FaceRight
      _ <- requestAnimationFrame win =<< animate win model
      raw <- RawNode <$> toJSVal elm
      _ <- forkIO $ threadDelay 1
        >> shpadoinkle id runSnabbdom model game (pure raw)
      return raw


view :: Html m a
view = baked $ (, retry) <$> play
