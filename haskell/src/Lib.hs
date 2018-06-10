{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ints2Lcd, Lcd(..), digits
    ) where

import           Prelude hiding (unlines)
import           Data.Foldable (fold)
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text, strip, unlines, intercalate)


data Lcd = Lcd [Text] deriving (Eq, Show)

instance Monoid Lcd where
  mempty = Lcd ["","",""]
  mappend (Lcd left) (Lcd right)= Lcd $ map (uncurry mappend) $ zip left right

ints2Lcd :: [Int] -> Maybe Text
ints2Lcd [] = Just ""
ints2Lcd input =
  lcdToString <$> fold <$> traverse digit2Lcd input
  where lcdToString (Lcd lcd) = intercalate "\n" lcd
        digit2Lcd n = M.lookup n digits

d0 = [
  " _ ",
  "| |",
  "|_|"]

d1 = [
  "   ",
  "  |",
  "  |"]

d2 = [
  " _ ",
  " _|",
  "|_ "]

d3 = [
  " _ ",
  " _|",
  " _|"]

d4 = [
  "   ",
  "|_|",
  "  |"]

d5 = [
  " _ ",
  "|_ ",
  " _|"]

d6 = [
  " _ ",
  "|_ ",
  "|_|"]

d7 = [
  " _ ",
  "  |",
  "  |"]

d8 = [
  " _ ",
  "|_|",
  "|_|"]

d9 = [
  " _ ",
  "|_|",
  " _|"]

digits :: M.Map Int Lcd
digits = M.fromList $ zip [0..] $ map Lcd [d0, d1, d2, d3, d4, d5, d6, d7, d8, d9]
