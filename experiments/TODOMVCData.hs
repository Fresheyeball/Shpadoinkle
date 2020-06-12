{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import           Control.Lens
import qualified Data.Map                      as M
import           Data.String
import           Data.Text                     hiding (all, count, filter,
                                                length)
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.Snabbdom
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo
import           Shpadoinkle.Lens

default (Text)

newtype Description = Description { unDescription :: Text } deriving (Show, Read, Eq, IsString, Semigroup, Monoid)
makeWrapped ''Description

newtype TaskId = TaskId { unTaskId :: Int } deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Completed = Complete
    | Incomplete
    deriving (Show, Read, Eq)

data Visibility = All
    | Active
    | Completed
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

data Task = Task
    { _description :: Description
    , _completed   :: Completed
    }
    deriving (Show, Read, Eq)
makeLenses ''Task

data Model = Model
    { _tasks      :: M.Map TaskId Task
    , _editing    :: Maybe TaskId
    , _visibility :: Visibility
    , _current    :: Description
    }
    deriving (Show, Read, Eq)
makeLenses ''Model

newTaskForm :: MonadJSM m => Model -> Html m Model
newTaskForm m = form [ className "todo-form", onSubmit . pur $ \m ->
  if m ^. current . to (== mempty) then m else m
    & tasks %~ insertMax (Task (m ^. current) Incomplete)
    & current .~ mempty ]
  [ input' [ className "new-todo"
           , m ^. current . _Wrapped . to value
           , onInput $ generalize current . pur . const . Description
           , placeholder "What needs to be done?" ]
  ]
  where insertMax x xs = M.insert k' x xs where k' = maybe minBound (succ . fst) $ M.lookupMax xs

todoList :: MonadJSM m => Model -> Html m Model
todoList m = ul "todo-list" . M.elems . M.mapWithKey (taskView m)
  $ m ^. tasks . to (case m ^. visibility of
    All       -> id
    Active    -> M.filter $ (== Incomplete) . _completed
    Completed -> M.filter $ (== Complete)   . _completed)

taskView :: MonadJSM m => Model -> TaskId -> Task -> Html m Model
taskView m = memo $ \tid (Task (Description d) c) ->
  li [ id' . pack . show $ unTaskId tid
     , className [ ("completed", c == Complete)
                 , ("editing", Just tid == m ^. editing) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ pur (& tasks . at tid . traverse . completed %~ (\case
                 Complete -> Incomplete
                 Incomplete -> Complete))
             , checked $ c == Complete
             ]
    , label [ onDblclick . generalize editing . pur $ const (Just tid) ] [ text d ]
    , button' [ className "destroy", onClick . pur $ (& tasks %~ M.delete tid) ]
    ]
  , form [ onSubmit . generalize editing . pur $ const Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ generalize (tasks . at tid) . maybeC . generalize description
                         . pur . const . Description
             , autofocus True
             , onBlur . generalize editing . pur $ const Nothing
             ]
    ]
  ]

listFooter :: Applicative m => Model -> Html m Model
listFooter m = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ [minBound..maxBound] & mapped %~ filterHtml (m ^. visibility)
                                        & fmap (generalize visibility)
  ] ++ (if count Complete (m ^. tasks) == 0 then [] else
  [ button [ className "clear-completed"
           , onClick . generalize tasks . pur $ M.filter ((== Incomplete) . _completed)
           ] [ "Clear completed" ]
  ])
  where count c = M.size . M.filter ((== c) . _completed)
        co = m ^. tasks . to (count Incomplete)

filterHtml :: Visibility -> Visibility -> Html' Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick (pur (const item)) : [className ("selected", cur == item)]) [ text . pack $ show item ]
  ]

info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]

toggleAllBtn :: Applicative m => Model -> [Html m Model]
toggleAllBtn m =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox",
             onChange $ generalize tasks . pur $ (& mapped . completed .~
               if m ^. tasks . to (all $ (== Complete) . _completed)
               then Incomplete else Complete) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]

view :: MonadJSM m => Model -> Html m Model
view m = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm m ]
    : htmlIfTasks
    [ section "main" $ toggleAllBtn m ++ [ todoList m ]
    , listFooter m
    ]
  , info
  ] where htmlIfTasks h' = if Prelude.null (m ^. tasks) then mempty else h'

app :: JSM ()
app = do
  m <- manageLocalStorage "todo" $ Model mempty Nothing All mempty
  initial <- readTVarIO m
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id runSnabbdom initial m Main.view getBody

main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app
