module Main (main) where

import Data.Bits as B

import Text.Printf
import Test.QuickCheck hiding ((.&.))

import Prelude hiding (length)
import Control.Arrow

import Genetic.Core
import Tests.Core

main :: IO ()
main = do
  putStrLn "\nTests start!"
  quickCheck prop_binaryInverseRev
  quickCheck prop_binaryInverseRangeRev
  quickCheck prop_binaryOffsetRev
  quickCheck prop_binaryOffsetRangeRev
