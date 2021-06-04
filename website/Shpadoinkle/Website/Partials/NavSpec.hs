module Shpadoinkle.Website.Partials.NavSpec (spec) where


import           Shpadoinkle.Website.Partials.Nav (toActive)
import           Shpadoinkle.Website.Types.Nav    (toRoute)
import           Test.Hspec                       (Spec, it)
import           Test.QuickCheck                  (property)


spec :: Spec
spec =
  it "nav should be active if we are on the represented route" . property $
    \n -> Just n == toActive (toRoute n)
