{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Marketing.Tailwind where


import           Shpadoinkle.Html.TH.CSS


$(extractNamespace "./static/tailwind.min.css")
