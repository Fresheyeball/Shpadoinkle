{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}


module Types.Prim where


import           Data.Aeson
import           Data.String
import           Data.Text
import           Database.Beam

#ifndef ghcjs_HOST_OS
import           Database.Beam.Backend.SQL.SQL92
import           Database.Beam.Sqlite
import           Database.Beam.Sqlite.Syntax
import           Database.SQLite.Simple.FromField
#endif

import           Servant.API                      hiding (Description)
import           Shpadoinkle.Widgets.Types


newtype SKU = SKU { unSKU :: Int  }
  deriving stock Generic
  deriving newtype (Real, Enum, Integral, Eq, Ord, Show, Read, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
#endif
instance Semigroup SKU where SKU x <> SKU y = SKU $ x + y
instance Monoid SKU where mempty = SKU 0


newtype Description = Description { unDescription  :: Text }
  deriving stock (Generic)
  deriving newtype (Eq, Ord, Show, Read, IsString, ToJSON, FromJSON, Humanize, Semigroup, Monoid)
  deriving anyclass (Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
#endif

instance Humanize (Maybe Description) where
  humanize = maybe "N/A" humanize


newtype SerialNumber = SerialNumber { unSerialNumber :: Int  }
  deriving stock Generic
  deriving newtype (Enum, Bounded, Real, Integral, Eq, Ord, Show, Num, ToJSON, FromJSON)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
#endif
instance Semigroup SerialNumber where SerialNumber x <> SerialNumber y = SerialNumber $ x + y
instance Monoid SerialNumber where mempty = SerialNumber 0


newtype SpaceCraftId = SpaceCraftId { unSpaceCraftId :: Int }
  deriving newtype ( Eq, Ord, Show, Num, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)
  deriving anyclass (Humanize, Present)
#ifndef ghcjs_HOST_OS
  deriving newtype (FromBackendRow Sqlite, HasSqlValueSyntax SqliteValueSyntax, HasSqlEqualityCheck Sqlite)
#endif


data Operable = Operational | Inoperable
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Humanize, Present, Generic, ToJSON, FromJSON)
#ifndef ghcjs_HOST_OS
  deriving (FromBackendRow Sqlite, HasSqlEqualityCheck Sqlite)
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Operable where sqlValueSyntax = autoSqlValueSyntax
#endif
instance Semigroup Operable where (<>) = min
instance Monoid Operable where mempty = maxBound


data Squadron = AwayTeam | StrikeForce | Scout
  deriving (Eq, Ord, Enum, Bounded, Read, Show, Present, Generic, ToJSON, FromJSON)
#ifndef ghcjs_HOST_OS
  deriving (FromBackendRow Sqlite)
instance HasSqlValueSyntax be String => HasSqlValueSyntax be Squadron where sqlValueSyntax = autoSqlValueSyntax
#endif
instance Semigroup Squadron where x <> _ = x


#ifndef ghcjs_HOST_OS
instance FromField Operable where fromField = fmap read <$> fromField
instance FromField Squadron where fromField = fmap read <$> fromField
#endif


instance Humanize Squadron where
  humanize = \case
    AwayTeam    -> "Away Team"
    StrikeForce -> "Strike Force"
    Scout       -> "Scouting"


instance Humanize (Maybe Squadron) where
  humanize = maybe "N/A" humanize
