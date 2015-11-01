module Genetic.Crossover where

import Genetic.Core

import Control.Monad
import Control.Monad.Random

import Prelude hiding (length)
import Data.List ((\\), foldl')

import qualified Data.HashSet as S

crsSinglePoint :: (MonadRandom m, FreeGenome g) => g -> g -> m (g, g)
crsSinglePoint g1 g2 = do
  splitPoint <- getRandomR (0, lasti)
  return $ swapRangeBetween (splitPoint, lasti) g1 g2
  where lasti = length g1 - 1

crsMultiPoint :: (MonadRandom m, FreeGenome g) => Rate -> g -> g -> m (g, g)
crsMultiPoint r g1 g2 = foldM mutate (g1, g2) [0..len - 1]
  where
    mutate (x, y) i = probValue r (x, y) $ swapBetween i i x y
    len = min (length g1) (length g2)

crsRandomRange :: (MonadRandom m, FreeGenome g) => g -> g -> m (g, g)
crsRandomRange g1 g2 = do
  begi <- getRandomR (0,    lasti)
  endi <- getRandomR (begi, lasti)
  return $ swapRangeBetween (begi, endi) g1 g2
  where lasti = length g1 - 1

crsPositionBased :: (MonadRandom m, FreeGenome g) => Rate -> g -> g -> m (g, g)
crsPositionBased r g1 g2 = do
  ps <- filterM (const $ (< r) <$> getRandomR (0, 1)) indices
  let rests = indices \\ ps
  let {
    process x ygs =
      let ss = S.fromList $ map (`getG` x) ps
          vs = filter (not . flip S.member ss) ygs
      in foldl' (flip (uncurry setG)) x $ zip rests vs
  }
  return (process g1 g2gs, process g2 g1gs)
  where
    len = min (length g1) (length g2)
    indices = [0..len - 1]
    g1gs = toList g1
    g2gs = toList g2
