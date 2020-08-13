{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Shpadoinkle.Html.TH.CSSTest
  ( pos_relative
  , id'foo
  , bar
  , foo
  , txt_rt
  ) where


import           Shpadoinkle.Html.TH.CSS


$(extractNamespace "./sample.css")
