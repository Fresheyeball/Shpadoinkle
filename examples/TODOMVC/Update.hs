{-# LANGUAGE OverloadedStrings #-}


module TODOMVC.Update where


import           TODOMVC.Types


appendItem :: Model -> Model
appendItem m = m
  { tasks = Task (current m) Incomplete ((+ 1)
          $ Prelude.maximum $ 0 : (taskId <$> tasks m)) : tasks m
  , current = "" }


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


toVisible :: Visibility -> [Task] -> [Task]
toVisible v = case v of
  All       -> id
  Active    -> filter $ (== Incomplete) . completed
  Completed -> filter $ (== Complete)   . completed
