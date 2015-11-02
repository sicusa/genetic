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

sclIdentity :: Generation g -> Vector (g, Score)
sclIdentity (Generation {..}) = V.zip genGenomes genScores

sclRanked :: Generation g -> Vector (g, Score)
sclRanked gen = V.imap (\i (g, _) -> (g, fromIntegral i)) sorted
  where
    len = fromIntegral $ V.length $ genGenomes gen
    scoredGs = sclIdentity gen
    sorted = runST $ do
      mv <- V.thaw scoredGs
      VA.sortBy (compare `on` snd) mv
      V.unsafeFreeze mv

sclSigma :: Generation g -> Vector (g, Score)
sclSigma (Generation {..}) =
  if stdDev == 0
    then (id &&& const 1) <$> genGenomes
    else V.zip genGenomes $ formula <$> genScores
  where
    len = fromIntegral $ V.length genGenomes
    average = genTotalScore / len
    stdDev = V.sum ((\x -> (x - average) ^ 2) <$> genScores) / len
    formula x = (x - average) / (2 * stdDev)

sclBoltzmann :: Double -> Double -> Generation g -> Vector (g, Score)
sclBoltzmann initTemp deltaTemp (Generation {..}) =
  V.zip genGenomes $ formula <$> genScores
  where
    currTemp = initTemp - fromIntegral genNum * deltaTemp
    len = fromIntegral $ V.length genGenomes
    average = genTotalScore / len
    formula x = (x / currTemp) / (average / currTemp)
