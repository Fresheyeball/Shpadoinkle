{-# LANGUAGE CPP #-}


module Main where


#ifndef ghcjs_HOST_OS

import           Control.Monad                  (replicateM)
import           Data.List                      (sortBy)
import           Data.Proxy
import           Network.Wai.Handler.Warp
import           Servant                        hiding (Stream)
import           Shpadoinkle.Widgets.Table      (SortCol (..), sortTable,
                                                 toFilter)
import           Shpadoinkle.Widgets.Table.Lazy (Length (..), Offset (..),
                                                 Page (..))
import           Test.QuickCheck

import           Types


numPeople :: Int
numPeople = 10000


arbitraryPersons :: IO [Person]
arbitraryPersons = do
  generate (replicateM numPeople arbitrary)


server :: [Person] -> Server Api
server = handler


handler :: Monad m => [Person] -> Page -> SortCol FilteredTable -> TableFilters -> m [Person]
handler persons (Page (Offset off) (Length len)) sc filts
  = return . take len . drop off . sortIt sc  $ filterBy filts persons


filterBy :: TableFilters -> [Person] -> [Person]
filterBy filts = filter (toFilter (FilteredTable (error "foo") filts) . PersonRow)


sortIt :: SortCol FilteredTable -> [Person] -> [Person]
sortIt sc = fmap unRow . sortBy (sortTable sc) . fmap PersonRow


api :: Proxy Api
api = Proxy


main :: IO ()
main = do
  persons <- arbitraryPersons
  run 8081 . serve api $ server persons

#else
main = putStrLn "server does not compile in ghcjs"
#endif
