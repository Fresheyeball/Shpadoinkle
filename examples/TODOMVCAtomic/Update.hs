{-# LANGUAGE OverloadedStrings #-}


module TODOMVCAtomic.Update where


-- import           Control.Lens

import           TODOMVCAtomic.Types


appendItem :: Description -> [Task] -> (Description, [Task])
appendItem d ts = if d /= "" then
  ("", Task d Incomplete ((+ 1)
          $ Prelude.maximum $ 0 : (_taskId <$> ts)) : ts)
  else (d, ts)


appendItem' :: Model -> Model
appendItem' m = if _current m /= "" then m
  { _tasks = Task (_current m) Incomplete ((+ 1)
          $ Prelude.maximum $ 0 : (_taskId <$> _tasks m)) : _tasks m
  , _current = "" }
  else m

toggleCompleted :: Model -> TaskId -> Model
toggleCompleted m tid = m { _tasks =
  (\t -> if _taskId t == tid then t { _completed = negC (_completed t) } else t) <$> _tasks m }
  where negC Complete   = Incomplete
        negC Incomplete = Complete


updateTaskDescription :: Model -> TaskId -> Description -> Model
updateTaskDescription m tid desc = m { _tasks = f <$> _tasks m}
  where f t = if _taskId t == tid then t { _description = desc } else t


toggleAll :: Model -> Model
toggleAll m = m { _tasks = (\t -> t { _completed = c }) <$> _tasks m}
  where c = if Prelude.all ((== Complete) . _completed) $ _tasks m then Incomplete else Complete


count :: Completed -> [Task] -> Int
count c = length . filter ((== c) . _completed)


clearComplete :: Model -> Model
clearComplete m = m { _tasks = filter ((== Incomplete) . _completed) (_tasks m) }


toVisible :: Visibility -> [Task] -> [Task]
toVisible v = case v of
  All       -> id
  Active    -> filter $ (== Incomplete) . _completed
  Completed -> filter $ (== Complete)   . _completed
