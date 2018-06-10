{-# LANGUAGE OverloadedStrings #-}
module Int2LcdUnitSpec where


import           Test.Hspec
import           Test.QuickCheck

import           Lib

spec :: Spec
spec = do
  describe "Int2Lcd Unit tests" $
    do it "empty list produces empty output" $
        (ints2Lcd []) `shouldBe` (Just "")
       it "input containing nondigits produces Nothing" $
        (ints2Lcd [10]) `shouldBe` Nothing
       it "input containing negative numbers produces Nothing" $
        (ints2Lcd [-1]) `shouldBe` Nothing
