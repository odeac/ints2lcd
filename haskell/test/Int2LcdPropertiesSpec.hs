module Int2LcdPropertiesSpec where

import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Set as Set
import           Test.Hspec
import           Test.QuickCheck

import           Lib

getDigit :: NonNegative Int -> Int
getDigit = flip rem 10 . getNonNegative

prop_checkLength :: NonEmptyList (NonNegative Int) -> Bool
prop_checkLength xs =
  (Text.length $ fromJust $ ints2Lcd $ digits) == expectedLength
  where digits = map getDigit $ getNonEmpty xs
        expectedLength = (length digits) * 9 + 2

prop_checkContent :: [NonNegative Int] -> Bool
prop_checkContent xs =
  and $ map isAllowed $ lcd
  where digits = map getDigit xs
        lcd = Text.unpack $ fromJust $ ints2Lcd digits
        charSet = Set.fromList [' ', '|', '_', '\n']
        isAllowed c = Set.member c charSet

prop_checkNonDigitPositiveInput :: NonEmptyList (NonNegative Int) -> Bool
prop_checkNonDigitPositiveInput xs = isNothing $ ints2Lcd $ ((+) 10) <$> getNonNegative <$> getNonEmpty xs

prop_checkNegativeInput :: NonEmptyList (Positive Int) -> Bool
prop_checkNegativeInput xs = isNothing $ ints2Lcd $ negate <$> getPositive <$> getNonEmpty xs

spec :: Spec
spec = do
  describe "Int2Lcd Properties" $
    do it "output length equals 9 * original length + 2" $ property $ prop_checkLength
       it "output contains only valid characters" $ property $ prop_checkContent
       it "non digit positive input produces Nothing" $ property $ prop_checkNonDigitPositiveInput
       it "negative input produces Nothing" $ property $ prop_checkNegativeInput
