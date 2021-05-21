module Shpadoinkle.Website.Types.CurrentYear (CurrentYear, getCurrentYear) where


import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Text                 (Text, pack)
import           Data.Time.Calendar        (toGregorian)
import           Data.Time.Clock           (getCurrentTime, utctDay)
import           Shpadoinkle.Widgets.Types (Humanize (..))


newtype CurrentYear = CurrentYear Text


getCurrentYear :: MonadIO m => m CurrentYear
getCurrentYear = liftIO $ do
  (year, _, _) <- toGregorian . utctDay <$> getCurrentTime
  return $ CurrentYear $ pack $ show year


instance Humanize CurrentYear where
  humanize (CurrentYear x) = x
