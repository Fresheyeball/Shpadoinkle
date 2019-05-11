{-# LANGUAGE OverloadedStrings #-}


module Main where

import           Control.Concurrent.STM.TVar
import           Control.Monad.IO.Class
import           Data.Text
import           Language.Javascript.JSaddle
import           Prelude                     hiding (div)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            (Html, a, className, div_, footer,
                                              form, h1_, header, href, input,
                                              li_, onInput', onSubmit, p_,
                                              placeholder, section, ul, value)


newtype Field       = Field       { unField       :: Text } deriving (Show, Eq)
newtype Description = Description { unDescription :: Text } deriving (Show, Eq)
data Completed = Complete | Incomplete deriving (Show, Eq)


data Task = Task
  { description :: Description
  , completed   :: Completed
  } deriving (Show, Eq)


data Visibility
  = All
  | Completed
  | Active
  deriving (Show, Eq)


data Model = Model
  { tasks      :: [Task]
  , visibility :: Visibility
  , current    :: Description
  } deriving (Show, Eq)


emptyModel :: Model
emptyModel = Model [] All (Description "")


appendItem :: Model -> Model
appendItem m = m { tasks = Task (current m) Incomplete : tasks m
                 , current = Description ""
                 }


updateDescription :: Model -> Description -> Model
updateDescription m d = m { current = d }


taskView :: Task -> Html m a
taskView (Task (Description d) _) = li_ [ text d ]


view :: MonadJSM m => Model -> Html m Model
view model = div_
  [ section "todoapp"
    [ header "header"
      [ h1_ [ "todos" ]
      , form [ className "todo-form", onSubmit $ do
          liftJSM $ print model
          return $ appendItem model
          ]
        [ input [ className "new-todo"
                , value . unDescription $ current model
                , onInput' $ updateDescription model . Description
                , placeholder "What needs to be done?" ] []
        ]
      ]
    , section "main"
      [ ul "todo-list" (taskView <$> tasks model)
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

