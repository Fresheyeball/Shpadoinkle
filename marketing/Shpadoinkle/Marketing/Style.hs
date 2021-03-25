{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Marketing.Style where


import           Shpadoinkle.Html.TH.CSS


$(extractNamespace "./static/style.css")
