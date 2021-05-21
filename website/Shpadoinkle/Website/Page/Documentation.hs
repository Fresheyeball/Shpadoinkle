{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


module Shpadoinkle.Website.Page.Documentation where


import           Data.Maybe                                     (maybeToList)
import           Data.Text                                      (Text, pack)
import           Prelude                                        hiding (div)
import           Shpadoinkle                                    (MonadJSM)
import           Shpadoinkle.Html                               as H
import           Shpadoinkle.Html.TH.AssetLink                  (assetLink)
import           Shpadoinkle.Template.TH                        (embedAsciidoc)
import           Shpadoinkle.Website.Style                      hiding (nav)
import           Shpadoinkle.Website.Types.Route                (Route (..))
import qualified Shpadoinkle.Website.Types.Route.GettingStarted as GettingStarted
import qualified Shpadoinkle.Website.Types.Route.Packages       as Packages
import qualified Shpadoinkle.Website.Types.Route.Tutorial       as Tutorial
import           Shpadoinkle.Website.Types.SPA                  (goTo)
import           Shpadoinkle.Widgets.Types                      (Humanize (..))


concepts :: MonadJSM m => Route -> Html m a
concepts = docsWrap ("Concepts" :: Text) $(embedAsciidoc "./docs/concepts.adoc")


gettingStarted :: MonadJSM m => GettingStarted.Route -> Route -> Html m a
gettingStarted r = docsWrap r $ case r of
  GettingStarted.RGSIndex               ->
    $(embedAsciidoc "./docs/getting-started.adoc")
  GettingStarted.RGSAddingToYourProject ->
    $(embedAsciidoc "./docs/getting-started/adding-to-your-project.adoc")
  GettingStarted.RGSExtendAnExample     ->
    $(embedAsciidoc "./docs/getting-started/extend-an-example.adoc")


packages :: MonadJSM m => Packages.Route -> Route -> Html m a
packages r = docsWrap r $ case r of
  Packages.RPIndex    -> $(embedAsciidoc "./docs/packages.adoc")
  Packages.RPCore     -> $(embedAsciidoc "./docs/packages/core.adoc")
  Packages.RPConsole  -> $(embedAsciidoc "./docs/packages/console.adoc")
  Packages.RPBackends -> $(embedAsciidoc "./docs/packages/backends.adoc")
  Packages.RPHtml     -> $(embedAsciidoc "./docs/packages/html.adoc")
  Packages.RPLens     -> $(embedAsciidoc "./docs/packages/lens.adoc")
  Packages.RPRouter   -> $(embedAsciidoc "./docs/packages/router.adoc")
  Packages.RPWidgets  -> $(embedAsciidoc "./docs/packages/widgets.adoc")


tutorial :: MonadJSM m => Tutorial.Route -> Route -> Html m a
tutorial r = docsWrap r $ case r of
  Tutorial.RTIndex              -> $(embedAsciidoc "./docs/tutorial.adoc")
  Tutorial.RTCalculator         -> $(embedAsciidoc "./docs/tutorial/calculator.adoc")
  Tutorial.RTImmediateExecution -> $(embedAsciidoc "./docs/tutorial/immediate-execution.adoc")
  Tutorial.RTComposing          -> $(embedAsciidoc "./docs/tutorial/composing.adoc")


top
  :: MonadJSM m
  => Route -> Route -> Text -> Maybe (Html m a, Html m a) -> Html m a
top cur r t mext = div [ class' side_nav__expand ] $
  [ a [ class' $ side_nav__expand__button <> asClass (side_nav__bold, r == cur)
      , goTo r
      ] $ [ text t ] <> maybeToList (fst <$> mext)
  ] <> maybeToList (snd <$> mext)


expandEq :: Route -> Route -> Bool
expandEq (RGettingStarted _) (RGettingStarted _) = True
expandEq (RPackages _) (RPackages _)             = True
expandEq (RTutorial _) (RTutorial _)             = True
expandEq x y                                     = x == y


expandable
  :: forall r m a
   . (MonadJSM m, Bounded r, Enum r, Humanize r)
  => Route -> (r -> Route) -> Text -> Html m a
expandable cur toR t = let topr = toR $ minBound @r in top cur topr t $ Just
  (H.span [ class' $ side_nav__expand__icon <> asClass (expanded, expandEq cur topr) ]
     [ img' [ src $(assetLink "/assets/expando.svg") ]
     ]
  , div [ class' side_nav__expand ]
    [ ul [ class' side_nav__expand__list ] $ sideNavLink cur toR <$> [ succ minBound .. maxBound @r ]
    | expandEq cur topr ]
  )


sideNavLink :: MonadJSM m => Humanize r => Route -> (r -> Route) -> r -> Html m a
sideNavLink cur toR r = li_
  [ a [ class' $ side_nav__link <> asClass (side_nav__bold, cur == toR r)
      , goTo (toR r) ] [ text $ humanize r ]
  ]


docsWrap :: (MonadJSM m, Humanize r) => r -> [Html m a] -> Route -> Html m a
docsWrap r content' cur = section [ class' flex ]
 [ nav [ class' side_nav ]
   [ top        cur RConcepts       "Basic Concept" Nothing
   , expandable cur RGettingStarted "Getting Started"
   , expandable cur RTutorial       "Tutorial"
   , expandable cur RPackages       "Reference"
   ]
 , h "main" [ class' docs ] $ h1_ [ text $ humanize r ] : content'
 ]
