{-# LANGUAGE OverloadedStrings #-}


module Widgets.DropdownSpec (spec) where


import           Control.Monad.IO.Class
import           Test.Hspec
import           Test.WebDriver


import           Util


spec :: Spec
spec = around_ (serve "widgets") $ do

  let getDrop x = do
        y <- findElem $ ById x
        header <- y `findElemFrom` ByClass "dropdown-toggle"
        expectClass y "dropdown"
        return (y, header)


  describe "Toggle does not effect selection" $ do

    itWD "One" $ do
      (one, oneHeader) <- getDrop "One"
      expectText oneHeader "Please Select"

      click one >> delay
      expectText oneHeader "Please Select"
      expectClass one "dropdown show"

      click one >> delay
      expectText oneHeader "Please Select"
      expectClass one "dropdown"


    itWD "AtleastOne" $ do
      (atleast, atleastHeader) <- getDrop "AtleastOne"
      expectText atleastHeader "Cheddar"

      click atleast >> delay
      expectText atleastHeader "Cheddar"
      expectClass atleast "dropdown show"

      click atleast >> delay
      expectText atleastHeader "Cheddar"
      expectClass atleast "dropdown"


  describe "Selecting should update the header" $ do

    itWD "One" $ do
      (one, oneHeader) <- getDrop "One"

      click one >> delay
      expectClass one "dropdown show"
      item:_ <- one `findElemsFrom` ByClass "dropdown-item"
      option <- getText item
      click item >> delay
      expectText oneHeader option
      expectClass one "dropdown"

      click one >> delay
      expectText oneHeader option
      expectClass one "dropdown show"

      click one >> delay
      expectText oneHeader option
      expectClass one "dropdown"


    -- itWD "AtleastOne" $ do
    --   (atleast, atleastHeader) <- getDrop "AtleastOne"

    --   click atleast >> delay
    --   expectClass atleast "dropdown show"
    --   item:_ <- atleast `findElemsFrom` ByClass "dropdown-item"
    --   option <- getText item
    --   click item >> delay
    --   expectText atleastHeader option
    --   expectClass atleast "dropdown"

    --   click atleast >> delay
    --   expectText atleastHeader option
    --   expectClass atleast "dropdown show"

    --   click atleast >> delay
    --   expectText atleastHeader option
    --   expectClass atleast "dropdown"


