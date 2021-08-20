{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}


module Shpadoinkle.Website.Types.SPA where


import           Data.Proxy                                     (Proxy (Proxy))
import           Data.Text                                      (Text, pack)
import           Servant.API                                    (type (:<|>) (..),
                                                                 type (:>))
import           Shpadoinkle                                    (MonadJSM, Prop,
                                                                 liftJSM,
                                                                 listenRaw)
import           Shpadoinkle.Continuation                       (done)
import           Shpadoinkle.Html                               (href,
                                                                 onClickM_,
                                                                 preventDefault,
                                                                 stopPropagation)
import           Shpadoinkle.Router                             (Redirect (..),
                                                                 Routed (..),
                                                                 View, getURI,
                                                                 mapUnions,
                                                                 navigate,
                                                                 type (:>>))
import           Shpadoinkle.Website.Types.Route                (Route (..))
import qualified Shpadoinkle.Website.Types.Route.GettingStarted as GettingStarted
import qualified Shpadoinkle.Website.Types.Route.Packages       as Packages
import qualified Shpadoinkle.Website.Types.Route.Tutorial       as Tutorial
import           Shpadoinkle.Website.Types.ViewModel            (ViewModel)


type View' m  = View m ViewModel


type SPA m
  =                                   View' m
  :<|> "docs" :> "index.html"      :> View' m

  :<|> "concepts"                  :> View' m
  :<|> "docs" :> "concepts.html"   :> View' m

  :<|> "getting-started"           :> GettingStarted.SPA       m ViewModel
  :<|> "docs" :> "getting-started" :> GettingStarted.LegacySPA m ViewModel

  :<|> "packages"                  :> Packages.SPA             m ViewModel
  :<|> "docs" :> "packages"        :> Packages.LegacySPA       m ViewModel

  :<|> "tutorial"                  :> Tutorial.SPA             m ViewModel
  :<|> "docs" :> "tutorial"        :> Tutorial.LegacySPA       m ViewModel

  :<|> "sandbox"                   :> View' m
  :<|> "404"                       :> View' m


routes :: SPA m :>> Route
routes
     = RHome
  :<|> RHome

  :<|> RConcepts
  :<|> RConcepts

  :<|> mapUnions RGettingStarted GettingStarted.routes
  :<|> mapUnions RGettingStarted GettingStarted.routes

  :<|> mapUnions RPackages       Packages.routes
  :<|> mapUnions RPackages       Packages.routes

  :<|> mapUnions RTutorial       Tutorial.routes
  :<|> mapUnions RTutorial       Tutorial.routes

  :<|> RSandbox
  :<|> RFourOhFour


instance Routed (SPA m) Route where
  redirect = \case
    RHome ->
      Redirect (Proxy @(View' m)) id

    RConcepts ->
      Redirect (Proxy @("concepts" :> View' m)) id

    RGettingStarted GettingStarted.RGSIndex ->
      Redirect (Proxy @("getting-started" :> View' m)) id
    RGettingStarted GettingStarted.RGSAddingToYourProject ->
      Redirect (Proxy @("getting-started" :> "adding-to-your-project" :> View' m)) id
    RGettingStarted GettingStarted.RGSExtendAnExample ->
      Redirect (Proxy @("getting-started" :> "extend-an-example" :> View' m)) id

    RPackages Packages.RPIndex ->
      Redirect (Proxy @("packages" :> View' m)) id
    RPackages Packages.RPCore ->
      Redirect (Proxy @("packages" :> "core" :> View' m)) id
    RPackages Packages.RPConsole ->
      Redirect (Proxy @("packages" :> "console" :> View' m)) id
    RPackages Packages.RPBackends ->
      Redirect (Proxy @("packages" :> "backends" :> View' m)) id
    RPackages Packages.RPHtml ->
      Redirect (Proxy @("packages" :> "html" :> View' m)) id
    RPackages Packages.RPLens ->
      Redirect (Proxy @("packages" :> "lens" :> View' m)) id
    RPackages Packages.RPRouter ->
      Redirect (Proxy @("packages" :> "router" :> View' m)) id
    RPackages Packages.RPWidgets ->
      Redirect (Proxy @("packages" :> "widgets" :> View' m)) id

    RTutorial Tutorial.RTIndex ->
      Redirect (Proxy @("tutorial" :> View' m)) id
    RTutorial Tutorial.RTCalculator ->
      Redirect (Proxy @("tutorial" :> "calculator" :> View' m)) id
    RTutorial Tutorial.RTImmediateExecution ->
      Redirect (Proxy @("tutorial" :> "immediate-execution" :> View' m)) id
    RTutorial Tutorial.RTComposing ->
      Redirect (Proxy @("tutorial" :> "composing" :> View' m)) id

    RSandbox ->
      Redirect (Proxy @("sandbox" :> View' m)) id

    RFourOhFour ->
      Redirect (Proxy @("404" :> View' m)) id


goTo :: forall m a. MonadJSM m => Route -> [(Text, Prop m a)]
#ifndef ghcjs_HOST_OS
goTo r = [href . ("/" <>) . pack . show $ getURI @(SPA m) r]
#else
goTo r =
  [ listenRaw "click" (const handleEvent)
  , href . ("/" <>) . pack . show $ getURI @(SPA m) r
  ]
  where
    handleEvent e = do
      liftJSM $ do
        preventDefault e
        stopPropagation e
      navigate @(SPA m) r
      pure done
#endif
