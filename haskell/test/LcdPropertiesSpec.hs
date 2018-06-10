module LcdPropertiesSpec where

import qualified Data.Text as T
import qualified Data.Set as Set
import           Test.Hspec
import           Test.Hspec.Checkers
import           Test.QuickCheck
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Checkers

import           Lib

instance Arbitrary Lcd where
  arbitrary = do
    top <- textArb
    mid <- textArb
    bottom <- textArb
    return $ Lcd [top, mid, bottom]
    where textArb :: Gen T.Text
          textArb = T.pack <$> vectorOf 3 (arbitrary :: Gen Char)

instance EqProp T.Text where
  left =-= right = (T.unpack left) =-= (T.unpack right)

instance EqProp Lcd where
  (Lcd left) =-= (Lcd right) = left =-= right

spec :: Spec
spec = do
  describe "Lcd properties" $ do
    it "Monoid laws for Lcd" $ do
      hspec $ testBatch (monoid (undefined:: Lcd))
