{-# LANGUAGE OverloadedStrings #-}


module TODOMVC.Update where


import           TODOMVC.Types


appendItem :: Model -> Model
appendItem m = if current m /= "" then m
  { tasks = Task (current m) Incomplete ((+ 1)
          $ Prelude.maximum $ 0 : (taskId <$> tasks m)) : tasks m
  , current = "" }
  else m


updateDescription :: Description -> Model -> Model
updateDescription d m = m { current = d }


toggleCompleted :: TaskId -> Model -> Model
toggleCompleted tid m = m { tasks =
  (\t -> if taskId t == tid then t { completed = negC (completed t) } else t) <$> tasks m }
  where negC Complete   = Incomplete
        negC Incomplete = Complete


toggleEditing :: Maybe TaskId -> Model -> Model
toggleEditing t m = m { editing = t }


updateTaskDescription :: TaskId -> Description -> Model -> Model
updateTaskDescription tid desc m = m { tasks = f <$> tasks m}
  where f t = if taskId t == tid then t { description = desc } else t


removeTask :: TaskId -> Model -> Model
removeTask tid m = m { tasks = filter ((/= tid) . taskId) $ tasks m}


toggleAll :: Model -> Model
toggleAll m = m { tasks = (\t -> t { completed = c }) <$> tasks m}
  where c = if Prelude.all ((== Complete) . completed) $ tasks m then Incomplete else Complete


count :: Completed -> [Task] -> Int
count c = length . filter ((== c) . completed)


clearComplete :: Model -> Model
clearComplete m = m { tasks = filter ((== Incomplete) . completed) (tasks m) }


toVisible :: Visibility -> [Task] -> [Task]
toVisible v = case v of
  All       -> id
  Active    -> filter $ (== Incomplete) . completed
  Completed -> filter $ (== Complete)   . completed
