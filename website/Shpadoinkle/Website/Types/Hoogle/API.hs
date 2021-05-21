{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}


module Shpadoinkle.Website.Types.Hoogle.API where


import           Data.Text                               (Text)
import           Servant.API                             (Get, JSON, QueryParam,
                                                          type (:>))
import           Shpadoinkle.Website.Types.Hoogle.Target (Target)


type HoogleAPI = QueryParam "mode"   Text
              :> QueryParam "hoogle" Text
              :> QueryParam "start"  Int
              :> QueryParam "count"  Int
              :> Get '[JSON] [Target]
