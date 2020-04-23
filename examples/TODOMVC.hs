{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Data.Text                     hiding (count, filter, length)
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html              hiding (main)
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo
import           Shpadoinkle.Html.Utils

import           TODOMVC.Types
import           TODOMVC.Update


default (Text)


filterHtml :: Applicative m => Visibility -> Visibility -> Html m Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick item : [className ("selected", cur == item)]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (tasks m) then [] else h'


taskView :: MonadJSM m => Model -> Task -> Html m Model
taskView m = memo $ \(Task (Description d) c tid) ->
  li [ id' . pack . show $ unTaskId tid
     , className [ ("completed", c == Complete)
                 , ("editing", Just tid == editing m) ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ toggleCompleted m tid
             , checked $ c == Complete
             ]
    , label [ onDblclick (toggleEditing m (Just tid)) ] [ text d ]
    , button' [ className "destroy", onClick (removeTask m tid) ]
    ]
  , form [ onSubmit $ toggleEditing m Nothing ]
    [ input' [ className "edit"
             , value d
             , onInput $ updateTaskDescription m tid . Description
             , autofocus True
             , onBlur $ toggleEditing m Nothing
             ]
    ]
  ]


listFooter :: Applicative m => Model -> Html m Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ fmap (\v -> model { visibility = v })
                <$> (filterHtml (visibility model) <$> [minBound..maxBound])
  ] ++ (if count Complete (tasks model) == 0 then [] else
  [ button [ className "clear-completed", onClick $ clearComplete model ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: MonadJSM m => Model -> Html m Model
newTaskForm model = form [ className "todo-form", onSubmit (appendItem model) ]
  [ input' [ className "new-todo"
           , value . unDescription $ current model
           , onInput $ updateDescription model . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: MonadJSM m => Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> toVisible (visibility model) (tasks model)


toggleAllBtn :: Applicative m => Model -> [Html m Model]
toggleAllBtn model =
  [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (toggleAll model) ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


view :: MonadJSM m => Model -> Html m Model
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
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id runParDiff emptyModel model view getBody


main :: IO ()
main = runJSorWarp 8080 app

