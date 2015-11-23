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

sclIdentity :: Monad m => Generation g -> m (Vector (g, Score))
sclIdentity (Generation {..}) = return $ V.zip genGenomes genScores

sclRanked :: Monad m => Generation g -> m (Vector (g, Score))
sclRanked (Generation {..}) =
    return $ V.imap (\i (g, _) -> (g, fromIntegral i)) sorted
  where
    sorted = runST $ do
      mv <- V.unsafeThaw $ V.zip genGenomes genScores
      VA.sortBy (compare `on` snd) mv
      V.unsafeFreeze mv

sclSigma :: Monad m => Generation g -> m (Vector (g, Score))
sclSigma (Generation {..}) =
  return $ if stdDev == 0
    then V.map (id &&& const 1) genGenomes
    else V.zip genGenomes $ V.map formula genScores
  where
    len = fromIntegral $ V.length genGenomes
    average = genTotalScore / len
    stdDev = V.sum ((\x -> (x - average) ^ 2) <$> genScores) / len
    formula x = (x - average) / (2 * stdDev)

sclBoltzmann :: Monad m => Double -> Double -> Generation g -> m (Vector (g, Score))
sclBoltzmann initTemp deltaTemp (Generation {..}) =
  return $ V.zip genGenomes $ formula <$> genScores
  where
    currTemp = initTemp - fromIntegral genCount * deltaTemp
    len = fromIntegral $ V.length genGenomes
    average = genTotalScore / len
    formula x = (x / currTemp) / (average / currTemp)
