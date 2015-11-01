module Genetic.ScoreScaling where

import Genetic.Core

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Tim as VA

import Control.Monad
import Control.Monad.ST
import Control.Arrow

import Data.Function
import Prelude hiding (length)

sclRanked :: ScoredGenomes g -> Vector (g, Score)
sclRanked sgs = V.imap (\i (g, _) -> (g, fromIntegral i)) sorted
  where
    gs = sgenomes sgs
    len = fromIntegral $ V.length gs
    sorted = runST $ do
      mv <- V.thaw gs
      VA.sortBy (compare `on` snd) mv
      V.unsafeFreeze mv

sclSigma :: ScoredGenomes g -> Vector (g, Score)
sclSigma sgs =
  if stdDev == 0
    then V.map (second $ const 1) gs
    else V.map (second formula) gs
  where
    gs  = sgenomes sgs
    total = stotal sgs
    len = fromIntegral $ V.length gs
    average = total / len
    stdDev = V.sum (V.map (\x -> (snd x - average) ^ 2) gs) / len
    formula x = (x - average) / (2 * stdDev)

sclBoltzmann :: Double -> Double -> ScoredGenomes g -> Vector (g, Score)
sclBoltzmann initTemp deltaTemp sgs = V.map (second formula) gs
  where
    gs = sgenomes sgs
    total = stotal sgs
    currTemp = initTemp - fromIntegral (sgeneration sgs) * deltaTemp
    len = fromIntegral $ V.length gs
    average = total / len
    formula x = (x / currTemp) / (average / currTemp)
