{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                  hiding (view)
import           Data.Generics.Labels          ()
import           Data.String
import           Data.Text                     hiding (count, filter, length)
import           GHC.Generics
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html
import           Shpadoinkle.Html.LocalStorage
import           Shpadoinkle.Html.Memo
import           Shpadoinkle.Lens


default (Text)


newtype Description = Description { unDescription :: Text } deriving (Generic, Show, Read, Eq, IsString)
newtype TaskId      = TaskId      { unTaskId      :: Int  } deriving (Generic, Show, Read, Eq, Ord, Num)


data Completed  = Complete | Incomplete
  deriving (Generic, Show, Read, Eq)


data Visibility = All | Active | Completed
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded)


data Task = Task
  { description :: Description
  , completed   :: Completed
  , taskId      :: TaskId
  } deriving (Generic, Show, Read, Eq)


data Model = Model
  { tasks      :: [Task]
  , editing    :: Maybe TaskId
  , visibility :: Visibility
  , current    :: Description
  } deriving (Generic, Show, Read, Eq)


emptyModel :: Model
emptyModel = Model [] Nothing All (Description "")


appendItem :: Model -> Model
appendItem m = if current m /= "" then m
  & #tasks   %~ (Task (current m) Incomplete newId :)
  & #current .~ "" else m
  where newId = Prelude.maximum (0 : (taskId <$> tasks m)) + 1


toggleCompleted :: TaskId -> Model -> Model
toggleCompleted tid m = m & #tasks . mapped %~ \t ->
  if taskId t == tid then t & #completed %~ negC else t
  where negC Complete   = Incomplete
        negC Incomplete = Complete


toggleEditing :: Maybe TaskId -> Model -> Model
toggleEditing t = #editing .~ t


updateTaskDescription :: TaskId -> Description -> Model -> Model
updateTaskDescription tid desc m = m & #tasks . mapped %~ \t ->
  if taskId t == tid then t & #description .~ desc else t


removeTask :: TaskId -> Model -> Model
removeTask tid m = m & #tasks %~ filter ((/= tid) . taskId)


toggleAll :: Model -> Model
toggleAll m = m & #tasks . traverse . #completed .~ c
  where c = if Prelude.all ((== Complete) . completed) $ tasks m then Incomplete else Complete


count :: Completed -> [Task] -> Int
count c = length . filter ((== c) . completed)


clearComplete :: Model -> Model
clearComplete = #tasks %~ filter ((== Incomplete) . completed)


toVisible :: Visibility -> [Task] -> [Task]
toVisible v = case v of
  All       -> id
  Active    -> filter $ (== Incomplete) . completed
  Completed -> filter $ (== Complete)   . completed


filterHtml :: Visibility -> Visibility -> Html m Visibility
filterHtml = memo2 $ \cur item -> li_
  [ a (href "#" : onClick item
        : [class' ("selected", cur == item)]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (tasks m) then [] else h'


taskView :: Model -> Task -> Html m Model
taskView m = memo $ \(Task (Description d) c tid) ->
  li [ id' . pack . show $ unTaskId tid
     , class' [ ("completed", c == Complete)
              , ("editing", Just tid == editing m)
              ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , class' "toggle"
             , onChange $ toggleCompleted tid m
             , checked $ c == Complete
             ]
    , label [ onDblclick $ toggleEditing (Just tid) m ] [ text d ]
    , button' [ class' "destroy", onClick $ removeTask tid m ]
    ]
  , form [ onSubmit $ toggleEditing Nothing m ]
    [ input' [ class' "edit"
             , value d
             , onInput $ ($ m) . updateTaskDescription tid . Description
             , autofocus True
             , onBlur $ toggleEditing Nothing m
             ]
    ]
  ]


listFooter :: Functor m => Model -> Html m Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ generalize #visibility .
      filterHtml (visibility model) <$> [minBound..maxBound]
  ] ++ (if count Complete (tasks model) == 0 then [] else
  [ button [ class' "clear-completed", onClick $ clearComplete model ] [ "Clear completed" ]
  ])



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: Model -> Html m Model
newTaskForm model = form [ class' "todo-form", onSubmit (appendItem model) ]
  [ input' [ class' "new-todo"
           , value . unDescription $ current model
           , onInput $ ($ model) . (#current .~) . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> visibility model `toVisible` tasks model


toggleAllBtn :: Model -> [Html m Model]
toggleAllBtn model =
  [ input' [ id' "toggle-all", class' "toggle-all", type' "checkbox", onChange $ toggleAll model ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


view :: Functor m => Model -> Html m Model
view model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm model ]
    : htmlIfTasks model
    [ section "main" $ toggleAllBtn model <> [ todoList model ]
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
