module Main where

import TestComponentGenOpSpec
import TestMultipleOutputs

main :: IO ()
main = do
  testComponentGenOpSpec
  testMultipleOutputs
