{-# LANGUAGE ScopedTypeVariables #-}

module Tests.Core where

import Genetic.Core
import Genetic.Instances

import Data.Word
import Debug.Trace
import Text.Printf

prop_binaryInverseRev (b :: BGenWrap Word) = b == b2
  where
    b1 = inverse b
    b2 = inverse b1

prop_binaryInverseRangeRev (b :: BGenWrap Word) = b == b2
  where
    b1 = inverseRange (10, 20) b
    b2 = inverseRange (10, 20) b1

prop_binaryOffsetRev (b :: BGenWrap Word) = b == b2
  where
    b1 = offset 5 32 b
    b2 = offset 37 (-32) b1

prop_binaryOffsetRangeRev (b :: BGenWrap Word) = b == b2
  where
    b1 = offsetRange (5, 10) 32 b
    b2 = offsetRange (37, 42) (-32) b1
