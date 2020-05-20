{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module TODOMVCAtomic.Types where


import           Control.Lens.TH
import           Data.String
import           Data.Text


newtype Description = Description { _unDescription :: Text } deriving (Show, Read, Eq, IsString)
newtype TaskId      = TaskId      { _unTaskId      :: Int  } deriving (Show, Read, Eq, Ord, Num)


makeWrapped ''Description
makeWrapped ''TaskId


data Completed  = Complete | Incomplete
  deriving (Show, Read, Eq)


makePrisms ''Completed


data Visibility = All | Active | Completed
  deriving (Show, Read, Eq, Ord, Enum, Bounded)


makePrisms ''Visibility


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
