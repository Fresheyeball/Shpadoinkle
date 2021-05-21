{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Website.Style where


import           Shpadoinkle.Html.TH.CSS


$(extractNamespace "./assets/style.css")
