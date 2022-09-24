{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Concurrent.STM                  (atomically, writeTVar)
import           Control.Monad                           (void, when)
import           Control.Monad.IO.Class                  (MonadIO (liftIO))
import           Data.Text                               (Text, pack)
import           Ease                                    (bounceOut)
import           GHCJS.DOM                               (currentWindowUnchecked)
import           GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
import           GHCJS.DOM.Window                        (Window,
                                                          requestAnimationFrame)
import           Shpadoinkle                             (Html, JSM, TVar,
                                                          newTVarIO,
                                                          shpadoinkle)
import           Shpadoinkle.Backend.Snabbdom            (runSnabbdom, stage)
import           Shpadoinkle.DeveloperTools              (withDeveloperTools)
import           Shpadoinkle.Html                        as H (div,
                                                               textProperty)
import           Shpadoinkle.Run                         (run)
import           UnliftIO.Concurrent                     (forkIO, threadDelay)


default (Text)


left :: Double -> Text
left x = "transform:translate3d(150px, " <> pack (show $ x / 5) <> "px, 0)"


easeRange :: Fractional b => b -> (b -> b) -> b -> b
easeRange r e = (* r) . e . (/ r)


dur :: Double
dur = 3000


view :: Double -> Html m a
view clock = H.div
  [ textProperty "style" $
     "position:absolute;background:red;padding:10px;" <> left
       (easeRange dur bounceOut clock) ]
  [ if | clock < 500        -> "Wat?"
       | clock < 1000       -> "AAAA!!"
       | clock < dur * 0.65 -> "Oh no!"
       | otherwise          -> "I'm ok" ]


wait :: Num n => n
wait = 3000000


animation :: Window -> TVar Double -> JSM ()
animation w t = void $ requestAnimationFrame w =<< go where
  go = newRequestAnimationFrameCallback $ \clock' -> do
    let clock = clock' - (wait / 1000)
    liftIO . atomically $ writeTVar t clock
    r <- go
    when (clock < dur) . void $ requestAnimationFrame w r


app :: JSM ()
app = do
  t <- newTVarIO 0
  withDeveloperTools t
  w <- currentWindowUnchecked
  _ <- forkIO $ threadDelay wait >> animation w t
  shpadoinkle id runSnabbdom t view stage


main :: IO ()
main = run app
