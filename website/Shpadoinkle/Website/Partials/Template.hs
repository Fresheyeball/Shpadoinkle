{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}


module Shpadoinkle.Website.Partials.Template
  ( wrapper
  , template
  ) where


import           Control.Lens                          ((&), (.~), (?~), (^.))
import           Data.Generics.Labels                  ()
import           Data.Text                             (Text, pack)
import           Network.URI                           (nullURIAuth)
import           Prelude                               hiding (div)
import           Shpadoinkle                           (MonadJSM)
import           Shpadoinkle.Html
import           Shpadoinkle.Html.TH.AssetLink         (assetLink)
import           Shpadoinkle.Lens                      (onRecord)
import           Shpadoinkle.Router                    (getURI, toHydration)
import           Shpadoinkle.Run                       (Env (..), entrypoint)
import           Shpadoinkle.Template.TH               (embedHtml)
import qualified Shpadoinkle.Website.Partials.Footer   as Footer
import qualified Shpadoinkle.Website.Partials.Nav      as Nav
import qualified Shpadoinkle.Website.Partials.Social   as Social
import           Shpadoinkle.Website.Types.CurrentYear (CurrentYear)
import           Shpadoinkle.Website.Types.PageModel   (PageModel)
import           Shpadoinkle.Website.Types.Route       (Route)
import           Shpadoinkle.Website.Types.SPA         (SPA)
import           Shpadoinkle.Website.Types.ViewModel   (ViewModel)
import           Shpadoinkle.Widgets.Types.Physical    (Toggle (..))


cdnjs :: Text
cdnjs = "https://cdnjs.cloudflare.com/ajax/libs/"
codemirrorCDN, highlightCDN :: Text -> Text
codemirrorCDN = mappend $ cdnjs <> "codemirror/5.59.4/"
highlightCDN  = mappend $ cdnjs <> "highlight.js/10.7.2/"


favicons :: [Html m a]
favicons = $(embedHtml "./favicon.html")


stylesheet :: Text -> Html m a
stylesheet x = link' [ rel "stylesheet", href x ]


javascript :: Text -> Html m a
javascript x = script [ src x ] []


headView :: forall m a. Env -> ViewModel -> Html m a
headView ev vm = head_ $
  [ meta' [ charset "UTF-8" ]
  , meta' [ content "width=device-width, initial-scale=1.0", name' "viewport" ]
  , stylesheet $ codemirrorCDN "codemirror.min.css"
  , stylesheet $ codemirrorCDN "theme/darcula.min.css"
  , stylesheet $ highlightCDN "styles/atom-one-light.min.css"
  , stylesheet $(assetLink "/assets/style.css")
  , stylesheet "https://stackpath.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
  , javascript $ entrypoint ev <> "?_=1"
  , javascript $ codemirrorCDN "codemirror.min.js"
  , javascript $ codemirrorCDN "mode/haskell/haskell.min.js"
  , javascript $ highlightCDN "highlight.min.js"
  , javascript $ highlightCDN "languages/haskell.min.js"
  , javascript $ highlightCDN "languages/bash.min.js"
  , javascript $ highlightCDN "languages/nix.min.js"
  , javascript $(assetLink "/assets/animate.js")
  , javascript $(assetLink "/assets/hljs.js")
  , meta' [ rel "sitemap", type' "application/xml", href "/sitemap.xml" ]
  , toHydration vm
  , link'
    [ rel "canonical"
    , href . pack . show
    . (#uriAuthority ?~ (nullURIAuth & #uriRegName .~ "shpadoinkle.org/"))
    . (#uriScheme .~ "https:")
    $ getURI @(SPA m) (vm ^. #currentRoute)
    ]
  ] <> Social.view (vm ^. #currentRoute) <> favicons


template :: Env -> ViewModel -> Html m a -> Html m a
template ev vm content' = html [ lang "en" ]
  [ headView ev vm
  , body "top-border" [ content' ]
  ]


wrapper :: MonadJSM m => CurrentYear -> Toggle -> Route -> Html m PageModel -> [Html m ViewModel]
wrapper yc t cur content'
  =  (onRecord #mobileMenu <$> Nav.view t cur)
  <> (onRecord #pageModel
 <$> case t of Open     -> []
               Closed _ ->[ content', Footer.view yc ])
