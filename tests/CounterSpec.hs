{-# LANGUAGE OverloadedStrings #-}


module CounterSpec (spec) where


import           Test.Hspec
import           Test.WebDriver

import           Util


spec :: Spec
spec = around_ (serve "counter") .
  itWD "increments and decrements" $ do
    di <- findElems (ByTag "button")
    let (dec, inc) = case di of [dec, inc] -> (dec, inc); _ -> error "bad pattern"
    out <- findElem (ById "out")
    expectText out "0"

    click inc
    expectText out "1"

    click inc
    expectText out "2"

    click dec
    expectText out "1"

    click dec
    expectText out "0"

    click dec
    expectText out "-1"
