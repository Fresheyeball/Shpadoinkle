{-# LANGUAGE OverloadedStrings #-}


module TODOSpec (spec) where


import           Control.Monad.IO.Class
import           Data.Text                  (unpack)

import           Test.Hspec
import           Test.WebDriver
import           Test.WebDriver.Common.Keys (end, enter)

import           Util


countItems :: WD Int
countItems = length <$> findElems (ByCSS ".todo-list li")


countTodos :: WD Int
countTodos = fmap (read . unpack) . getText =<<
  findElem (ByCSS ".todo-count strong")


spec :: Spec
spec = around_ (serve "todomvc") $ do

  itWD "basic crud" $ do

    -- CREATE
    input <- findElem $ ByClass "new-todo"
    equals 0 =<< countItems

    let buyACat   = "buy a cat"
        andNameIt = " and name it mittens"

    sendKeysSlowly (buyACat <> enter) input
    delay

    -- READ
    equals 1 =<< countItems
    item <- findElem $ ById "1"
    expectText item buyACat

    -- COMPLETE
    moveToCenter item
    equals (Just "") =<< item `attr` "class"
    delay
    equals 1 =<< countTodos
    check <- findElem $ ByClass "toggle"
    click check
    delay
    equals (Just "completed") =<< item `attr` "class"
    click check
    delay
    equals 1 =<< countTodos
    equals (Just "") =<< item `attr` "class"

    -- EDIT
    moveToCenter item
    doubleClick
    delay
    edit <- findElem $ ByClass "edit"
    sendKeysSlowly (end <> andNameIt <> enter) edit
    delay
    expectText item (buyACat <> andNameIt)

    -- DELETE
    delete <- findElem $ ByClass "destroy"
    click delete
    delay

    equals 0 =<< countItems


  itWD "filters" $ do

    input <- findElem $ ByClass "new-todo"
    sendKeysSlowly ("a" <> enter) input >> delay
    sendKeysSlowly ("b" <> enter) input >> delay
    equals 2 =<< countItems
    equals 2 =<< countTodos

    [ _, check ] <- findElems $ ByClass "toggle"
    [ all, active, completed ] <- findElems $ ByCSS ".filters a"

    click check
    delay

    equals 2 =<< countItems
    equals 1 =<< countTodos

    click active
    delay >> delay
    equals 1 =<< countItems
    b <- findElem $ ById "2"
    expectText b "b"

    click completed
    delay >> delay
    equals 1 =<< countItems
    a <- findElem $ ById "1"
    expectText a "a"

    click all
    delay
    equals 2 =<< countItems
    [ check', _ ] <- findElems $ ByClass "toggle"
    click check'
    delay
    equals 2 =<< countItems
    equals 0 =<< countTodos
