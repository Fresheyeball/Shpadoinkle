{-# LANGUAGE ApplicativeDo        #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults   #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}


module Shpadoinkle.Website.View where


import           Control.Lens                              ((%~), (^.), (^?))
import           Control.Monad.IO.Class                    (MonadIO, liftIO)
import           Control.Monad.Reader
import           Data.Generics.Labels                      ()
import           Data.Text                                 (Text)
import           Prelude                                   hiding (div)
import           Shpadoinkle
import           Shpadoinkle.Html                          as H
import           Shpadoinkle.Isreal.Types                  as Swan
import           Shpadoinkle.Lens                          (onSum)
import           Shpadoinkle.Website.Component.LiveExample
import qualified Shpadoinkle.Website.Page.Documentation    as Documentation
import qualified Shpadoinkle.Website.Page.FourOhFour       as FourOhFour
import qualified Shpadoinkle.Website.Page.Home             as Home
import           Shpadoinkle.Website.Partials.Template
import           Shpadoinkle.Website.Style                 (documentation)
import           Shpadoinkle.Website.Types.CurrentYear
import           Shpadoinkle.Website.Types.Effects.Example
import           Shpadoinkle.Website.Types.Home
import           Shpadoinkle.Website.Types.PageModel
import           Shpadoinkle.Website.Types.Route
import           Shpadoinkle.Website.Types.Social          (toSocial)
import           Shpadoinkle.Website.Types.ViewModel
import           UnliftIO.Async                            (Concurrently (..),
                                                            async)


default (Text)


topClass :: Route -> ClassList
topClass = \case
  RHome -> mempty
  _     -> documentation


view :: (MonadJSM m, ExampleEffects m) => CurrentYear -> ViewModel -> Html m ViewModel
view cy vm = div [ class' $ topClass cur ] . wrapper cy (vm ^. #mobileMenu) cur $
  case (cur, vm ^. #pageModel) of
    (RHome,               MHome x) -> onSum #_MHome $ Home.view x
    (RConcepts,           MStatic) -> Documentation.concepts            cur
    (RGettingStarted gsr, MStatic) -> Documentation.gettingStarted gsr  cur
    (RPackages pr,        MStatic) -> Documentation.packages       pr   cur
    (RTutorial tutr,      MStatic) -> Documentation.tutorial       tutr cur
    (RSandbox,            MStatic) -> ""
    (RFourOhFour, _)               -> FourOhFour.view
    _                              -> FourOhFour.view
    where cur = vm ^. #currentRoute


compileExamples :: forall m. (MonadUnliftIO m, MonadJSM m, ExampleEffects m) => TVar ViewModel -> m ()
compileExamples model = runConcurrently $ foldl (*>) (pure ())
  [ compileWith #helloWorld
  , compileWith #counter
  , compileWith #todo
  ]
  where
  compileWith :: ExampleLens -> Concurrently m ()
  compileWith l = Concurrently $ do
    vm <- readTVarIO model
    case vm ^? #pageModel . #_MHome . #examples . l of
      Nothing -> pure ()
      Just x  -> do
        f <- runContinuation (compileExample l) x
        atomically $ modifyTVar model (#pageModel . #_MHome . #examples . l %~ f)


genExampleTokens :: MonadIO m => m (Examples SnowToken)
genExampleTokens = liftIO $ Examples <$> genSnowToken <*> genSnowToken <*> genSnowToken


start :: MonadIO m => Route -> m ViewModel
start r = ViewModel mempty r <$> case r of
  RHome -> MHome . emptyHome <$> genExampleTokens
  _     -> pure MStatic


startJS
  :: (MonadUnliftIO m, MonadJSM m, ExampleEffects m)
  => TVar ViewModel -> Route -> m ViewModel
startJS model r = do
  vm <- start r
  setTitle $ toSocial r ^. #title
  async $ compileExamples model
  return vm
