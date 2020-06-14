{-# LANGUAGE CPP                  #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                  (set, to, (%~), (.~), (?~), (^.),
                                                _Wrapped)
import           Data.Text                     hiding (count, filter, length)
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.Snabbdom
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo

import           TODOMVCAtomic.Types
import           TODOMVCAtomic.Update


default (Text)


filterHtml :: Eq v => Show v => v -> v -> Html' v
filterHtml = memo $ \cur item -> li_
  [ a [href "#" , onClick' item , className [("selected", cur == item)]] [ text . pack $ show item ]
  ]


htmlIfTasks :: [b] -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null m then [] else h'


taskView :: MonadJSM m => Maybe TaskId -> Task -> Html m Model
taskView = memo $ \ed (Task (Description d) c tid) ->
  li [ id' . pack . show $ tid ^. _Wrapped
     , className [ ("completed", c == Complete)
                 , ("editing",   Just tid == ed) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange . pur $ toggleCompleted tid
             , checked $ c == Complete
             ]
    , label [ onDblclick . pur $ editing ?~ tid ] [ text d ]
   , button' [ className "destroy", onClick . pur $ tasks %~ filter ((/= tid) . _taskId) ]
    ]
  , form [ onSubmit . pur $ editing .~ Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ pur . updateTaskDescription tid . Description
             , autofocus True
             , onBlur . pur $ editing .~ Nothing
             ]
    ]
  ]


listFooter :: Applicative m => Int -> Int -> Visibility -> Html m Model
listFooter = memo $ \ic cc v -> footer "footer" $
  [ Shpadoinkle.Html.span "todo-count"
    [ strong_ [ text . pack $ show ic ]
    , text $ " item" <> (if ic == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ constly (set visibility) . filterHtml v <$> [minBound..maxBound]
  ] ++ (if cc == 0 then [] else
  [ button [ className "clear-completed", onClick (pur clearComplete) ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Description -> Html m Model
newTaskForm = memo $ \desc -> form [ className "todo-form", onSubmit (pur appendItem') ]
  [ input' [ className "new-todo"
           , value $ desc ^. _Wrapped
           , onInput $ pur . set current . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: MonadJSM m => Maybe TaskId -> Visibility -> [Task] -> Html m Model
todoList = memo $ \ed v ts -> ul "todo-list" $ taskView ed <$> toVisible v ts


toggleAllBtn :: Applicative m => [Html m Model]
toggleAllBtn =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (pur toggleAll) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


render :: MonadJSM m => Model -> Html m Model
render m = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm $ m ^. current ]
    : htmlIfTasks (m ^. tasks)
    [ section "main" $ toggleAllBtn ++ [ todoList (m ^. editing) (m ^. visibility) (m ^. tasks) ]
    , listFooter (m ^. tasks . to (count Incomplete))
                 (m ^. tasks . to (count Complete))
                 (m ^. visibility)
    ]
  , info
  ]


app :: JSM ()
app = do
  model <- manageLocalStorage "todo" emptyModel
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  initial <- readTVarIO model
  shpadoinkle id runSnabbdom initial model render stage


main :: IO ()
main = runJSorWarp 8080 app

