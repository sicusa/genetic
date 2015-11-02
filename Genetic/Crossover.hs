module Genetic.Crossover where

import Genetic.Core

import Control.Monad
import Control.Monad.Random

import Prelude hiding (length)
import Data.List ((\\), foldl')
import Data.Maybe (fromJust)

import qualified Data.Vector as V
import qualified Data.HashSet as S
import Data.Hashable (Hashable)

crsPartiallyMapped :: (MonadRandom m, PermutationGenome g, Eq (Gene g)) => g -> g -> m (g, g)
crsPartiallyMapped g1 g2 = do
  begi <- getRandomR (0,    lasti)
  endi <- getRandomR (begi, lasti)
  let is  = V.enumFromTo begi endi
      vs1 = flip getG g1 <$> is
      vs2 = flip getG g2 <$> is
      ng1 = swapMulti (V.imap (\i v -> (fromJust $ findG v g1, i)) vs2) g1
      ng2 = swapMulti (V.imap (\i v -> (fromJust $ findG v g2, i)) vs1) g2
  return (ng1, ng2)
  where lasti = length g1 - 1

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

crsPositionBased :: (MonadRandom m, FreeGenome g, Gene g ~ e, Eq e, Hashable e) => Rate -> g -> g -> m (g, g)
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
