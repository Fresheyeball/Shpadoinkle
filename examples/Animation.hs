{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Concurrent.STM                  (atomically, writeTVar)
import           Control.Monad                           (void)
import           Control.Monad.IO.Class                  (MonadIO (liftIO))
import           Data.Text                               (Text, pack)
import           Ease                                    (bounceOut)
import           GHCJS.DOM                               (currentWindowUnchecked)
import           GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallback)
import           GHCJS.DOM.Window                        (Window,
                                                          requestAnimationFrame)
import           Shpadoinkle                             (Html, JSM, TVar,
                                                          newTVarIO,
                                                          runJSorWarp,
                                                          shpadoinkle)
import           Shpadoinkle.Backend.Snabbdom            (runSnabbdom, stage)
import           Shpadoinkle.Html                        as H (div,
                                                               textProperty)


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


animation :: Window -> TVar Double -> JSM ()
animation w t = void $ requestAnimationFrame w =<< go where
  go = newRequestAnimationFrameCallback $ \clock -> do
    liftIO . atomically $ writeTVar t clock
    r <- go
    if clock < dur then void $ requestAnimationFrame w r else return ()


main :: IO ()
main = do
  putStrLn "\nHappy point of view on https://localhost:8080\n"
  runJSorWarp 8080 $ do
    t <- newTVarIO 0
    w <- currentWindowUnchecked
    animation w t
    shpadoinkle id runSnabbdom 0 t view stage
