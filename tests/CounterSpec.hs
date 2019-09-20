{-# LANGUAGE OverloadedStrings #-}


module CounterSpec (spec) where


import           Control.Monad.IO.Class

import           Test.Hspec
import           Test.WebDriver

import           Util


spec :: Spec
spec = around_ (serve "counter") .
  itWD "increments and decrements" $ do
    [dec, inc]<- findElems (ByTag "button")
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
