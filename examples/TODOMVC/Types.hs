{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module TODOMVC.Types where


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
