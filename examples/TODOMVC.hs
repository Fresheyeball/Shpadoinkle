{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                  hiding (view)
import           Data.Text                     hiding (count, filter, length)
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo

import           TODOMVC.Types
import           TODOMVC.Update


default (Text)


filterHtml :: Visibility -> Visibility -> Html' Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick' item
        : [className ("selected", cur == item)]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (_tasks m) then [] else h'


taskView :: MonadJSM m => Model -> Task -> Html m Model
taskView m = memo $ \(Task (Description d) c tid) ->
  li [ id' . pack . show $ unTaskId tid
     , className [ ("completed", c == Complete)
                 , ("editing", Just tid == _editing m) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange . pur $ toggleCompleted tid
             , checked $ c == Complete
             ]
    , label [ onDblclick . pur . toggleEditing $ Just tid ] [ text d ]
    , button' [ className "destroy", onClick . pur $ removeTask tid ]
    ]
  , form [ onSubmit . pur $ toggleEditing Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ pur . updateTaskDescription tid . Description
             , autofocus True
             , onBlur . pur $ toggleEditing Nothing
             ]
    ]
  ]


listFooter :: Applicative m => Model -> Html m Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ _tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ constly (set visibility) . filterHtml (_visibility model) <$> [minBound..maxBound]
  ] ++ (if count Complete (_tasks model) == 0 then [] else
  [ button [ className "clear-completed", onClick (pur clearComplete) ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Model -> Html m Model
newTaskForm model = form [ className "todo-form", onSubmit (pur appendItem) ]
  [ input' [ className "new-todo"
           , value . unDescription $ _current model
           , onInput $ pur . set current . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: MonadJSM m => Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> toVisible (_visibility model) (_tasks model)


toggleAllBtn :: Applicative m => [Html m Model]
toggleAllBtn =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (pur toggleAll) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


view :: MonadJSM m => Model -> Html m Model
view model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm model ]
    : htmlIfTasks model
    [ section "main" $ toggleAllBtn ++ [ todoList model ]
    , listFooter model
    ]
  , info
  ]


app :: JSM ()
app = do
  model <- manageLocalStorage "todo" emptyModel
  initial <- readTVarIO model
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id runParDiff initial model view getBody


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app

