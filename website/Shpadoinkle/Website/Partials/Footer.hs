{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Website.Partials.Footer (view) where


import           Prelude                               hiding (div)
import           Shpadoinkle                           (MonadJSM)
import           Shpadoinkle.Html                      (Html, a, alt, class',
                                                        div, div_, footer_,
                                                        href, img', li_, nav_,
                                                        newTab, p, src, text,
                                                        ul)
import           Shpadoinkle.Html.TH.AssetLink         (assetLink)
import           Shpadoinkle.Website.Style             as Style
import           Shpadoinkle.Website.Types.CurrentYear (CurrentYear)
import           Shpadoinkle.Website.Types.Nav         (Nav, toRoute)
import           Shpadoinkle.Website.Types.Route       (Route (RHome))
import           Shpadoinkle.Website.Types.SPA         (goTo)
import           Shpadoinkle.Widgets.Types             (humanize)


footerLink :: MonadJSM m => Nav -> Html m a
footerLink n = li_
  [ a (goTo $ toRoute n)
    [ text $ humanize n ]
  ]


view :: MonadJSM m => CurrentYear -> Html m a
view cy =
  footer_
    [ div [ class' footer__wrapper ]
      [ a (goTo RHome)
        [ img' [ alt "Shpadoinkle", src $(assetLink "/assets/try_shpadoinkle_footer_logo.svg") ]
        ]
      , div [ class' $ flex <> flex_col <> justify_between <> w_full <> gap_8 <> pt_12 <> leading_8 <> "md:flex-row" <> "md:w-2/5" <> "md:gap-0" ]
        [ div_
          [ p [ class' footer__nav__category ]
            [ "Quick Links" ]
          , nav_
            [ ul [ class' footer__nav__links ] $ footerLink <$> [minBound..maxBound]
            ]
          ]
        , div_
          [ p [ class' footer__nav__category ]
            [ "Community" ]
          , nav_
            [ ul [ class' footer__nav__links ]
              [ li_
                [ a [ newTab, href "https://shpadoinkle.zulipchat.com" ]
                  [ "Zulip" ]
                ]
              , li_
                [ a [ newTab, href "https://twitter.com/ShpadoinkleUI" ]
                  [ "Twitter" ]
                ]
              , li_
                [ a [ newTab, href "https://gitlab.com/platonic/shpadoinkle" ]
                  [ "GitLab" ]
                ]
              ]
            ]
          ]
        ]
      , div [ class' footer_copyright ]
        [ text $ "© 2020–" <> humanize cy <> " Platonic Systems Limited"
        , img' [ alt "scarf tracking", src "https://static.scarf.sh/a.png?x-pxid=a8bc4a6f-783f-4d1b-a41d-4dd180a27dbd" ]
        ]
      ]
    ]
