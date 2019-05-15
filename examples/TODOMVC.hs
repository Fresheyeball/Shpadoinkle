{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import qualified Data.Set                    as Set
import           Data.Text
import           Language.Javascript.JSaddle
import           Prelude                     hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            hiding (main)


import           Debug.Trace


newtype Field       = Field       { unField       :: Text } deriving (Show, Eq)
newtype Description = Description { unDescription :: Text } deriving (Show, Eq)
newtype TaskId      = TaskId      { unTaskId      :: Int  } deriving (Show, Eq, Ord, Num)
data Completed = Complete | Incomplete deriving (Show, Eq)


data Task = Task
  { description :: Description
  , completed   :: Completed
  , taskId      :: TaskId
  } deriving (Show, Eq)


data Visibility
  = All
  | Completed
  | Active
  deriving (Show, Eq)


data Model = Model
  { tasks      :: [Task]
  , editing    :: Maybe TaskId
  , visibility :: Visibility
  , current    :: Description
  } deriving (Show, Eq)


emptyModel :: Model
emptyModel = Model [] Nothing All (Description "")


appendItem :: Model -> Model
appendItem m = m { tasks = Task (current m) Incomplete ((+ 1) $ Prelude.maximum $ 0 : (taskId <$> tasks m)) : tasks m
                 , current = Description ""
                 }


updateDescription :: Model -> Description -> Model
updateDescription m d = m { current = d }


toggleCompleted :: Model -> TaskId -> Model
toggleCompleted m tid = m { tasks =
  (\t -> if taskId t == tid then t { completed = negC (completed t) } else t) <$> tasks m }
  where negC Complete   = Incomplete
        negC Incomplete = Complete


toggleEditing :: Model -> Maybe TaskId -> Model
toggleEditing m t = m { editing = t }


updateTaskDescription :: Model -> TaskId -> Description -> Model
updateTaskDescription m tid desc = m { tasks = f <$> tasks m}
  where f t = if taskId t == tid then t { description = desc } else t


removeTask :: Model -> TaskId -> Model
removeTask m tid = m { tasks = Prelude.filter ((/= tid) . taskId) $ tasks m}


toggleAll :: Model -> Model
toggleAll m = m { tasks = (\t -> t { completed = trace ("c: " ++ show c) c }) <$> tasks m}
  where c = if Prelude.all ((== Complete) . completed) $ tasks m then Incomplete else Complete


taskView :: MonadJSM m => Model -> Task -> Html m Model
taskView m (Task (Description d) c tid) =
  li [ id' . pack . show $ unTaskId tid
     , className . Set.fromList $ [ "completed" | c == Complete ] ++ [ "editing" | Just tid == editing m ]
     ]
  [ div "view"
    [ input' [ type' "checkbox"
             , className "toggle"
             , onChange $ toggleCompleted m tid
             , checked (trace (show c) $ c == Complete)
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


view :: MonadJSM m => Model -> Html m Model
view model = div_
  [ section "todoapp"
    [ header "header"
      [ h1_ [ "todos" ]
      , form [ className "todo-form", onSubmit $ appendItem model ]
        [ input' [ className "new-todo"
                 , value . unDescription $ current model
                 , onInput $ updateDescription model . Description
                 , placeholder "What needs to be done?" ]
        ]
      ]
    , section "main"
      [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange $ toggleAll model ]
      , label [ for' "toggle-all" ] [ "Mark all as complete" ]
      , ul "todo-list" (taskView model <$> tasks model)
      ]
    ]
  , footer "info"
    [ p_ [ "Double-click to edit a todo" ]
    , p_ [ "Part of", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
    ]
  ]


addStyle :: String -> String
addStyle x =
  "const l = document.createElement('link') \n" ++
  "l.href = '" ++ x ++ "' \n" ++
  "l.rel = 'stylesheet' \n" ++
  "document.head.appendChild(l)"


main :: IO ()
main = do
  model <- newTVarIO emptyModel
  _ <- eval $ addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  _ <- eval $ addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id (runParDiff model) model view . fmap RawNode $ liftIO $

    eval ("document.body" :: String)

