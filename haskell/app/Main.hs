{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Prelude hiding(putStr, putStrLn, length)
import Data.Maybe
import Data.Text.IO (putStr, putStrLn)
import Data.Text (length)

main :: IO ()
main = do
  putStrLn "\n [0..9] as LCD:"
  putStrLn "'"
  putStr $ fromJust $ ints2Lcd [0..9]
  putStrLn "'"
  print $ ints2Lcd [11]
  print $ ints2Lcd [-1]
