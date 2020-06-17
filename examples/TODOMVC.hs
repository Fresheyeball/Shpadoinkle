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


filterHtml :: Visibility -> Visibility -> Html Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick item
        : [className ("selected", cur == item)]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [h a] -> [h a]
htmlIfTasks m h' = if Prelude.null (_tasks m) then [] else h'


taskView :: Model -> Task -> Html Model
taskView m = memo $ \(Task (Description d) c tid) ->
  li [ id' . pack . show $ unTaskId tid
     , className [ ("completed", c == Complete)
                 , ("editing", Just tid == _editing m) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ toggleCompleted tid m
             , checked $ c == Complete
             ]
    , label [ onDblclick $ toggleEditing (Just tid) m ] [ text d ]
    , button' [ className "destroy", onClick $ removeTask tid m ]
    ]
  , form [ onSubmit $ toggleEditing Nothing m ]
    [ input' [ className "edit"
             , value d
             , onInput $ ($ m) . updateTaskDescription tid . Description
             , autofocus True
             , onBlur $ toggleEditing Nothing m
             ]
    ]
  ]


listFooter :: Model -> Html Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ _tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ fmap (($ model) . (visibility .~)) <$> (filterHtml (_visibility model) <$> [minBound..maxBound])
  ] ++ (if count Complete (_tasks model) == 0 then [] else
  [ button [ className "clear-completed", onClick (clearComplete model) ] [ "Clear completed" ]
  ])



info :: Html a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: Model -> Html Model
newTaskForm model = form [ className "todo-form", onSubmit (appendItem model) ]
  [ input' [ className "new-todo"
           , value . unDescription $ _current model
           , onInput $ ($ model) . (current .~) . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: Model -> Html Model
todoList model = ul "todo-list" $ taskView model <$> toVisible (_visibility model) (_tasks model)


toggleAllBtn :: Model -> [Html Model]
toggleAllBtn m =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (toggleAll m) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


view :: Model -> Html Model
view model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm model ]
    : htmlIfTasks model
    [ section "main" $ toggleAllBtn model ++ [ todoList model ]
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
  shpadoinkle id runParDiff initial model (constly' . view) getBody


main :: IO ()
main = do
  putStrLn "running app"
  runJSorWarp 8080 app

