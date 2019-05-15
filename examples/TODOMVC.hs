{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}


module Main where

import           Control.Concurrent
import           Control.Concurrent.STM.TVar
import           Control.Monad               (void)
import           Control.Monad.IO.Class
import           Control.Monad.STM
import           Data.Maybe                  (fromMaybe)
import qualified Data.Set                    as Set
import           Data.Text                   hiding (count, filter, length)
import           Language.Javascript.JSaddle
import           Prelude                     hiding (div, unwords)
import           Shpadoinkle
import           Shpadoinkle.Backend.ParDiff
import           Shpadoinkle.Html            hiding (main)
import           Text.Read                   (readMaybe)


newtype Field       = Field       { unField       :: Text } deriving (Show, Read, Eq)
newtype Description = Description { unDescription :: Text } deriving (Show, Read, Eq)
newtype TaskId      = TaskId      { unTaskId      :: Int  } deriving (Show, Read, Eq, Ord, Num)


data Completed  = Complete | Incomplete
  deriving (Show, Read, Eq)


data Visibility = All | Active | Completed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


data Task = Task
  { description :: Description
  , completed   :: Completed
  , taskId      :: TaskId
  } deriving (Show, Read, Eq)


data Model = Model
  { tasks      :: [Task]
  , editing    :: Maybe TaskId
  , visibility :: Visibility
  , current    :: Description
  } deriving (Show, Read, Eq)


emptyModel :: Model
emptyModel = Model [] Nothing All (Description "")


appendItem :: Model -> Model
appendItem m = m
  { tasks = Task (current m) Incomplete ((+ 1)
          $ Prelude.maximum $ 0 : (taskId <$> tasks m)) : tasks m
  , current = Description "" }


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
removeTask m tid = m { tasks = filter ((/= tid) . taskId) $ tasks m}


toggleAll :: Model -> Model
toggleAll m = m { tasks = (\t -> t { completed = c }) <$> tasks m}
  where c = if Prelude.all ((== Complete) . completed) $ tasks m then Incomplete else Complete


count :: Completed -> [Task] -> Int
count c = length . filter ((== c) . completed)


clearComplete :: Model -> Model
clearComplete m = m { tasks = filter ((== Incomplete) . completed) (tasks m) }


htmlIfTasks :: Model -> [Html m a] -> [Html m a]
htmlIfTasks m h' = if Prelude.null (tasks m) then [] else h'


filterHtml :: Applicative m => Visibility -> Visibility -> Html m Visibility
filterHtml cur item = li_
  [ a (href "#" : onClick item : [className "selected" | cur == item]) [ text . pack $ show item ]
  ]


toVisible :: Visibility -> [Task] -> [Task]
toVisible v = case v of
  All       -> id
  Active    -> filter $ (== Incomplete) . completed
  Completed -> filter $ (== Complete)   . completed


taskView :: MonadJSM m => Model -> Task -> Html m Model
taskView m (Task (Description d) c tid) =
  li [ id' . pack . show $ unTaskId tid
     , className . Set.fromList $ [ "completed" | c == Complete ] ++ [ "editing" | Just tid == editing m ]
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


view :: MonadJSM m => Model -> Html m Model
view model = div_
  [ section "todoapp" $
    header "header"
      [ h1_ [ "todos" ]
      , form [ className "todo-form", onSubmit (appendItem model) ]
        [ input' [ className "new-todo"
                 , value . unDescription $ current model
                 , onInput $ updateDescription model . Description
                 , placeholder "What needs to be done?" ]
        ]
      ]
    : htmlIfTasks model
    [ section "main"
      [ input' [ id' "toggle-all", className "toggle-all", type' "checkbox", onChange (toggleAll model) ]
      , label [ for' "toggle-all" ] [ "Mark all as complete" ]
      , ul "todo-list" $ taskView model <$> toVisible (visibility model) (tasks model)
      ]
    , footer "footer" $
      [ Shpadoinkle.Html.span "todo-count" $ let co = count Incomplete $ tasks model in
        [ strong_ [ text . pack $ show co ]
        , text $ " item" <> (if co == 1 then "" else "s") <> " left"
        ]
      , ul "filters" $ fmap (\v -> model { visibility = v })
                    <$> (filterHtml (visibility model) <$> [minBound..maxBound])
      ] ++ (if count Complete (tasks model) == 0 then [] else
      [ button [ className "clear-completed", onClick $ clearComplete model ] [ "Clear completed" ]
      ])
    ]
  , footer "info"
    [ p_ [ "Double-click to edit a todo" ]
    , p_ [ "Credits ", a [ href "https://twitter.com/fresheyeball" ] [ "Isaac Shapira" ] ]
    , p_ [ "Part of ", a [ href "http://todomvc.com" ] [ "TodoMVC" ] ]
    ]
  ]


addStyle :: String -> IO ()
addStyle x = void . eval $
  "const l = document.createElement('link') \n" ++
  "l.href = '" ++ x ++ "' \n" ++
  "l.rel = 'stylesheet' \n" ++
  "document.head.appendChild(l)"


setStorage :: Model -> IO ()
setStorage m = void . eval $ "localStorage.setItem('todo', " ++ show (show m) ++ ")"


getStorage :: IO (Maybe Model)
getStorage = do
  x <- fromJSVal =<< eval ("localStorage.getItem('todo')" :: Text)
  return $ readMaybe =<< x


saveOnChange :: TVar Model -> TVar Model -> IO ()
saveOnChange old new' = do
  n <- atomically $ do
    o <- readTVar old
    n <- readTVar new'
    if o == n then retry else n <$ writeTVar old n
  setStorage n
  saveOnChange old new'


manageLocalStorage :: IO (TVar Model)
manageLocalStorage = do
  model <- newTVarIO . fromMaybe emptyModel =<< getStorage
  old   <- newTVarIO =<< readTVarIO model
  void . forkIO $ saveOnChange old model
  return model


main :: IO ()
main = do
  model <- manageLocalStorage
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-common@1.0.5/base.css"
  addStyle "https://cdn.jsdelivr.net/npm/todomvc-app-css@2.2.0/index.css"
  shpadoinkle id (runParDiff model) model view . fmap RawNode $ liftIO $
    eval ("document.body" :: String)

