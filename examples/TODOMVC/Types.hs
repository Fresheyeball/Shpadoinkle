{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module TODOMVC.Types where


import           Control.Lens
import           Data.String
import           Data.Text


newtype Description = Description { unDescription :: Text } deriving (Show, Read, Eq, IsString)
newtype TaskId      = TaskId      { unTaskId      :: Int  } deriving (Show, Read, Eq, Ord, Num)


data Completed  = Complete | Incomplete
  deriving (Show, Read, Eq)


data Visibility = All | Active | Completed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


data Task = Task
  { _description :: Description
  , _completed   :: Completed
  , _taskId      :: TaskId
  } deriving (Show, Read, Eq)
makeLenses ''Task


data Model = Model
  { _tasks      :: [Task]
  , _editing    :: Maybe TaskId
  , _visibility :: Visibility
  , _current    :: Description
  } deriving (Show, Read, Eq)
makeLenses ''Model


emptyModel :: Model
emptyModel = Model [] Nothing All (Description "")
