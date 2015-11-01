module Genetic.Mutation where

import Genetic.Core

import Control.Monad
import Control.Monad.Random

import qualified Data.Vector as V
import Prelude hiding (length)

mutMultiPoint :: (MonadRandom m, FreeGenome g, Gene g e) => (e -> e) -> Rate -> g -> m g
mutMultiPoint f r g = foldM mutateBit g [0..length g - 1]
  where mutateBit a p = probValue r a $ setG p (f $ getG p a) a

mutScramble :: (MonadRandom m, PermutationGenome g) => Rate -> g -> m g
mutScramble r g = probEvent r (return g) $ do
  begi <- getRandomR (0, lasti - 2)
  endi <- getRandomR (begi, lasti)
  let selecti = getRandomR (begi, endi)
  swapPairs <- replicateM (endi - begi) $ (,) <$> selecti <*> selecti
  return $ foldl (flip $ uncurry swap) g swapPairs
  where lasti = length g - 1

mutDispacement :: (MonadRandom m, PermutationGenome g) => Int -> Rate -> g -> m g
mutDispacement minRange r g = probEvent r (return g) $ do
  begi <- getRandomR (0, lasti - minRange)
  endi <- min minRange <$> getRandomR (begi, lasti)
  offs <- getRandomR (-begi, lasti - endi)
  return $ offsetRange (begi, endi) offs g
  where lasti = length g - 1

mutInsertion :: (MonadRandom m, PermutationGenome g) => Rate -> g -> m g
mutInsertion r g = probEvent r (return g) $ do
  i <- getRandomR (0, lasti)
  offs <- getRandomR (-i, lasti - i)
  return $ offset i offs g
  where lasti = length g - 1

mutInversion :: (MonadRandom m, PermutationGenome g) => Rate -> g -> m g
mutInversion r g = probEvent r (return g) $ do
  begi <- getRandomR (0,    lasti)
  endi <- getRandomR (begi, lasti)
  return $ if begi == endi
    then g
    else inverseRange (begi, endi) g
  where lasti = length g - 1

mutDisplacedInversion :: (MonadRandom m, PermutationGenome g) => Rate -> g -> m g
mutDisplacedInversion r g = probEvent r (return g) $ do
  begi <- getRandomR (0,    lasti)
  endi <- getRandomR (begi, lasti)
  let newg = if begi == endi then g else inverseRange (begi, endi) g
  offs <- getRandomR (-begi, lasti - endi)
  return $ offsetRange (begi, endi) offs newg
  where lasti = length g - 1
