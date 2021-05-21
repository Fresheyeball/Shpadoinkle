{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Website.Partials.Nav (view) where


import           Prelude                            hiding (div)
import           Shpadoinkle                        (MonadJSM)
import           Shpadoinkle.Html                   as H
import           Shpadoinkle.Html.TH.AssetLink
import           Shpadoinkle.Website.Style          as Style
import           Shpadoinkle.Website.Types.Nav
import           Shpadoinkle.Website.Types.Route
import           Shpadoinkle.Website.Types.SPA      (goTo)
import           Shpadoinkle.Widgets.Types
import           Shpadoinkle.Widgets.Types.Physical (Toggle (..))


toActive :: Route -> Maybe Nav
toActive = \case
  RHome             -> Just NHome
  RGettingStarted _ -> Just NGettingStarted
  RTutorial       _ -> Just NTutorial
  -- RSandbox          -> Just NSandbox
  _                 -> Nothing


navLinkDesktop :: MonadJSM m => Route -> Nav -> Html m a
navLinkDesktop r n = li [ class' nav__link ] $
  a [ goTo $ toRoute n ] [ text $ humanize n ]
  : [ img' [ class' $ if r == RHome then nav__link_underline else nav__link__underline_docs
           , src $(assetLink "/assets/nav_line.svg") ] | toActive r == Just n ]


navLinkMobile :: MonadJSM m => Nav -> Html m a
navLinkMobile n = a [ class' nav_mobile__link, goTo $ toRoute n ] [ text $ humanize n ]


view :: MonadJSM m => Toggle -> Route -> [Html m Toggle]
view t r = pure $ div [ class' started_header ] $
  [ H.nav [ class' Style.nav ]
    [ a [ goTo $ toRoute NHome ]
      [ img' [ class' nav__logo, src $(assetLink "/assets/landing_logo.svg") ]
      ]
    , img' [ class' nav__menu_icon, onClick toggle, src $ case t of
        Closed _ -> $(assetLink "/assets/menu_icon.svg")
        Open     -> $(assetLink "/assets/menu_close_icon.svg")
      ]
    , ul [ class' nav__links ] $ navLinkDesktop r <$> [minBound..maxBound]
    ]
  ] <> case t of
      Closed _ -> []
      Open     -> [ H.nav [ class' nav_mobile ]
        [ div [ class' nav_mobile__wrapper ] $ navLinkMobile <$> [minBound..maxBound] ]
        ]
