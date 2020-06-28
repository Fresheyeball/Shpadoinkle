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


filterHtml :: Eq v => Show v => v -> v -> Html v
filterHtml = memo $ \cur item -> li_
  [ a [href "#" , onClick item , class' [("selected", cur == item)]] [ text . pack $ show item ]
  ]


htmlIfTasks :: [b] -> [h a] -> [h a]
htmlIfTasks m h' = if Prelude.null m then [] else h'


taskView :: Monad m => Maybe TaskId -> Task -> HtmlM m Model
taskView = memo $ \ed (Task (Description d) c tid) ->
  li [ id' . pack . show $ tid ^. _Wrapped
     , class' [ ("completed", c == Complete)
                 , ("editing",   Just tid == ed) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , class' "toggle"
             , onChangeE . pur $ toggleCompleted tid
             , checked $ c == Complete
             ]
    , label [ onDblclickE . pur $ editing ?~ tid ] [ text d ]
   , button' [ class' "destroy", onClickE . pur $ tasks %~ filter ((/= tid) . _taskId) ]
    ]
  , form [ onSubmitE . pur $ editing .~ Nothing ]
    [ input' [ class' "edit"
             , value d
             , onInputE $ pur . updateTaskDescription tid . Description
             , autofocus True
             , onBlurE . pur $ editing .~ Nothing
             ]
    ]
  ]


listFooter :: Monad m => Int -> Int -> Visibility -> HtmlM m Model
listFooter = memo $ \ic cc v -> footer "footer" $
  [ Shpadoinkle.Html.span "todo-count"
    [ strong_ [ text . pack $ show ic ]
    , text $ " item" <> (if ic == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ constly (set visibility) . filterHtml v <$> [minBound..maxBound]
  ] ++ (if cc == 0 then [] else
  [ button [ class' "clear-completed", onClickE (pur clearComplete) ] [ "Clear completed" ]
  ])



info :: Monad m => HtmlM m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: Monad m => Description -> HtmlM m Model
newTaskForm = memo $ \desc -> form [ class' "todo-form", onSubmitE (pur appendItem') ]
  [ input' [ class' "new-todo"
           , value $ desc ^. _Wrapped
           , onInputE $ pur . set current . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: Monad m => Maybe TaskId -> Visibility -> [Task] -> HtmlM m Model
todoList = memo $ \ed v ts -> ul "todo-list" $ taskView ed <$> toVisible v ts


toggleAllBtn :: Monad m => [HtmlM m Model]
toggleAllBtn =
  [ input' [ id' "toggle-all", class' "toggle-all", type' "checkbox", onChangeE (pur toggleAll) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


render :: Monad m => Model -> HtmlM m Model
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

