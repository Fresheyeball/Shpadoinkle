{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedLabels           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Main where


import           Control.Lens                  hiding (view)
import           Data.Generics.Labels          ()
import           Data.String                   (IsString)
import           Data.Text                     hiding (count, filter, length)
import           GHC.Generics                  (Generic)
import           Prelude                       hiding (div, unwords)
import           Shpadoinkle                   (Html, JSM, NFData, shpadoinkle,
                                                text)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Html              (a, addStyle, autofocus, button,
                                                button', checked, class', div,
                                                div_, footer, for', form, h1_,
                                                header, href, id', input',
                                                label, li, li_, onBlur,
                                                onChange, onClick, onDblclick,
                                                onInput, onSubmit, p_,
                                                placeholder, section, span,
                                                strong_, type', ul, value)
import           Shpadoinkle.Html.LocalStorage (manageLocalStorage)
import           Shpadoinkle.Lens              (onRecord)
import           Shpadoinkle.Run               (run)


default (Text)


newtype Description = Description { unDescription :: Text }
  deriving stock Generic deriving newtype (Show, Read, Eq, IsString) deriving anyclass NFData
newtype TaskId      = TaskId      { unTaskId      :: Int  }
  deriving stock Generic deriving newtype (Show, Read, Eq, Ord, Num) deriving anyclass NFData


data Completed  = Complete | Incomplete
  deriving (Generic, Show, Read, Eq, NFData)


data Visibility = All | Active | Completed
  deriving (Generic, Show, Read, Eq, Ord, Enum, Bounded, NFData)


data Task = Task
  { description :: Description
  , completed   :: Completed
  , taskId      :: TaskId
  } deriving (Generic, Show, Read, Eq, NFData)


data Model = Model
  { tasks      :: [Task]
  , editing    :: Maybe TaskId
  , visibility :: Visibility
  , current    :: Description
  } deriving (Generic, Show, Read, Eq, NFData)


emptyModel :: Model
emptyModel = Model [] Nothing All (Description "")


appendItem :: Model -> Model
appendItem m = if current m /= "" then m
  & #tasks   %~ (<> [Task (current m) Incomplete newId])
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
filterHtml cur item = li_
  [ a (href "#" : onClick (const item)
        : [class' ("selected", cur == item)]) [ text . pack $ show item ]
  ]


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (tasks m) then [] else h'


taskView :: Model -> Task -> Html m Model
taskView m (Task (Description d) c tid) =
  li [ id' . pack . show $ unTaskId tid
     , class' [ ("completed", c == Complete)
              , ("editing", Just tid == editing m)
              ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , class' "toggle"
             , onChange $ toggleCompleted tid
             , checked $ c == Complete
             ]
    , label [ onDblclick $ toggleEditing (Just tid) ] [ text d ]
    , button' [ class' "destroy", onClick $ removeTask tid ]
    ]
  , form [ onSubmit $ toggleEditing Nothing ]
    [ input' [ class' "edit"
             , value d
             , onInput $ updateTaskDescription tid . Description
             , autofocus True
             , onBlur $ toggleEditing Nothing
             ]
    ]
  ]


listFooter :: Functor m => Model -> Html m Model
listFooter model = footer "footer" $
  [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ tasks model in
    [ strong_ [ text . pack $ show co ]
    , text $ " item" <> (if co == 1 then "" else "s") <> " left"
    ]
  , ul "filters" $ onRecord #visibility .
      filterHtml (visibility model) <$> [minBound..maxBound]
  ] ++ [ button [class' "clear-completed", onClick clearComplete ] ["Clear completed"]
       | count Complete (tasks model) /= 0 ]



info :: Html m a
info = footer "info"
  [ p_ [ "Double-click to edit a todo" ]
  , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
  , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
  ]


newTaskForm :: Model -> Html m Model
newTaskForm model = form [ class' "todo-form", onSubmit appendItem ]
  [ input' [ class' "new-todo"
           , value . unDescription $ current model
           , onInput $ (#current .~) . Description
           , placeholder "What needs to be done?" ]
  ]


todoList :: Model -> Html m Model
todoList model = ul "todo-list" $ taskView model <$> visibility model `toVisible` tasks model


toggleAllBtn :: [Html m Model]
toggleAllBtn =
  [ input' [ id' "toggle-all", class' "toggle-all", type' "checkbox", onChange toggleAll ]
  , label [ for' "toggle-all" ] [ "Mark all as complete" ]
  ]


view :: Functor m => Model -> Html m Model
view model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ], newTaskForm model ]
    : htmlIfTasks model
    [ section "main" $ toggleAllBtn <> [ todoList model ]
    , listFooter model
    ]
  , info
  ]


app :: JSM ()
app = do
  model <- manageLocalStorage "todo" emptyModel
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id runSnabbdom model view stage


main :: IO ()
main = do
  putStrLn "running app"
  run app
