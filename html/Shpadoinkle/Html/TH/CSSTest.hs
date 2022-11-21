{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -ddump-splices #-}


module Shpadoinkle.Html.TH.CSSTest
  ( pos_relative
  , id'foo
  , bar
  , foo
  , txt_rt
  , woah
  ) where


import           Shpadoinkle.Html.TH.CSS


$(extractNamespace "./sample.css")
