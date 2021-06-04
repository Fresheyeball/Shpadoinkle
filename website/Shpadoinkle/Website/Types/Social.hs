{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}


module Shpadoinkle.Website.Types.Social where


import           Data.Text                       (Text)
import           GHC.Generics                    (Generic)
import           Shpadoinkle.Website.Types.Route (Route (..))


data Social = Social
  { title       :: Text
  , description :: Text
  , image       :: Maybe Text
  } deriving Generic


toSocial :: Route -> Social
toSocial = prependTitle . \case
  RHome             -> Social ""
    "A new Functional UI programming paradigm" Nothing

  RConcepts         -> Social "Concept"
    "Learn the basic ideas need to write Shpadoinkle applications" Nothing

  RGettingStarted _ -> Social "Getting Started"
    "Instructions for getting stared using Shpadoinkle for application development" Nothing

  RPackages _       -> Social "Reference"
    "Packages in the Shpadoinkle ecosystem" Nothing

  RTutorial _       -> Social "Tutorial"
    "Step by step guide, writing a simple Shpadoinkle project" Nothing

  _                 -> Social "404"
    "This page is not found" Nothing


prependTitle :: Social -> Social
prependTitle s = s { title = "Shpadoinkle" <> f (title s) }
  where f "" = ""
        f x  = " | " <> x
