{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Genetic.Core where

import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST
import Control.Monad.Trans.Control
import Control.Monad.Reader
import Control.Concurrent.Async.Lifted

import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

import Data.Foldable (foldl', foldr')

import Prelude hiding (length)

-- | The 'GenomeBase' class represents possible solutions for a given problem.
--
class GenomeBase g where
  type Gene g :: *
  -- | Get the amount of genes.
  length :: g -> Int
  -- | Convert from a list.
  fromList :: [Gene g] -> g
  -- | Convert to a list.
  toList :: g -> [Gene g]
  -- | Get a gene.
  getG :: Int -> g -> Gene g
  -- | Find the index of a given gene, return Nothing if the gene does not exist.
  findG :: (Gene g ~ e, Eq e) => e -> g -> Maybe Int

  {-# INLINE toList #-}
  toList g = map (`getG` g) [0..length g - 1]

  {-# INLINE findG #-}
  findG v g = search 0
    where
      search i
        | i == len      = Nothing
        | getG i g == v = Just i
        | otherwise     = search $ i + 1
      len = length g

-- | Genomes that only support permutation operators,
-- used for problems which valid permutation is required for a feasible solution,
-- e.g. Traveling Salesman Problem.
--
class GenomeBase g => PermutationGenome g where
  -- | Swap two genes in one genome.
  swap :: Int -> Int -> g -> g
  -- | Swap multiple genes.
  swapMulti :: Foldable t => t (Int, Int) -> g -> g
  -- | Reverse a range of genomes.
  inverseRange :: (Int, Int) -> g -> g
  -- | Inverse the entire genome.
  inverse :: g -> g
  -- | Offset a range of genes.
  offsetRange :: (Int, Int) -> Int -> g -> g
  -- | Offset the gene.
  offset :: Int -> Int -> g -> g

  {-# INLINE swapMulti #-}
  swapMulti ips g = foldr' (uncurry swap) g ips

  {-# INLINE offset #-}
  offset i = offsetRange (i, i)

  inverseRange (beg, end) g =
    foldl' (\a x -> swap x (end - x) a) g [0 .. total `div` 2]
    where total = end - beg

  {-# INLINE inverse #-}
  inverse g = inverseRange (0, length g - 1) g

-- | 'FreeGenome' genes can be modified without restrictions, so that valid
-- permutation is not guaranteed.
--
class PermutationGenome g => FreeGenome g where
  -- | Modify a gene with specific value.
  setG :: Gene g ~ e => Int -> e -> g -> g
  -- | Swap two genes between two given genomes.
  swapBetween :: Int -> Int -> g -> g -> (g, g)
  -- | Swap a range between two given genomes.
  swapRangeBetween :: (Int, Int) -> g -> g -> (g, g)

  {-# INLINE swapBetween #-}
  swapBetween i1 i2 g1 g2 =
    let v1 = getG i1 g1
        v2 = getG i2 g2
    in (setG i1 v2 g1, setG i2 v1 g2)

  {-# INLINE swapRangeBetween #-}
  swapRangeBetween (beg, end) g1 g2 =
    foldl' (\(ga, gb) i -> swapBetween i i ga gb) (g1, g2) [beg..end]

-- | Genomes which support binary operators.
--
class FreeGenome g => BinaryGenome g where
  shift :: Int -> g -> g
  gand  :: g -> g -> g
  gor   :: g -> g -> g
  gxor  :: g -> g -> g

-- | Genomes that can be expaneded to increase the complexity and diversity of
-- solutions.
class FreeGenome g => ExpandableGenome g where
  concat :: g -> g -> g
  repeat :: (Int, Int) -> Int -> g -> g

type Probability = Double
type Score = Double

data GeneticSettings u = GeneticSettings
  { gsCrossoverProb    :: Probability
  , gsMutationProb     :: Probability
  , gsPopulation       :: Int
  , gsUserData         :: u }

data Generation g = Generation
  { genGenomes    :: Vector g
  , genScores     :: Vector Score
  , genTotalScore :: Score
  , genBestGenome :: (g, Score)
  , genCount      :: Int }

data GeneticOperators g m = GeneticOperators
  { scoreMarkOpr  :: Vector g -> m (Vector Score)
  , scoreScaleOpr :: Generation g -> m (Vector (g, Score))
  , selectionOpr  :: Int -> (g -> g -> m (g, g)) -> Vector (g, Score) -> m (Vector (g, g))
  , crossoverOpr  :: g -> g -> m (g, g)
  , mutationOpr   :: g -> m g
  , predicateOpr  :: Generation g -> Bool }

{-# INLINE probEvent #-}
probEvent :: MonadRandom m => Probability -> m g -> m g -> m g
probEvent r def m = do
  rf <- getRandomR (0, 1)
  if rf > r then def else m

{-# INLINE probValue #-}
probValue :: MonadRandom m => Probability -> g -> g -> m g
probValue r def slt = do
  rf <- getRandomR (0, 1)
  return $ if rf > r then def else slt

mapPairM :: Monad m => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (a, b) = (,) <$> f a <*> f b

runGenetic
  :: (GenomeBase g, MonadRandom m, MonadBaseControl IO m, s ~ GeneticSettings u)
  => s -> GeneticOperators g (ReaderT s m) -> Vector g -> m (Generation g)
runGenetic settings oprs initgs = runReaderT (runGeneticBase oprs initgs) settings

runGeneticBase
  :: (GenomeBase g, MonadRandom m, MonadBaseControl IO m, MonadReader (GeneticSettings u) m)
  => GeneticOperators g m -> Vector g -> m (Generation g)
runGeneticBase (GeneticOperators{..}) initgs = do
  settings <- ask
  let population = gsPopulation settings
      crossoverProb = gsCrossoverProb settings
      mutationProb = gsMutationProb settings

  let probCrossoverOpr g1 g2 = probEvent crossoverProb (return (g1, g2)) $ crossoverOpr g1 g2
      probMutationOpr g = probEvent mutationProb (return g) $ mutationOpr g
      crsAndMut g1 g2 = probCrossoverOpr g1 g2 >>= mapPairM probMutationOpr

      loop generation gs = do
        scores <- scoreMarkOpr gs
        let best = (V.unsafeIndex gs &&& V.unsafeIndex scores) $ V.maxIndex scores
            gen = Generation
              { genGenomes    = gs
              , genScores     = scores
              , genTotalScore = V.sum scores
              , genBestGenome = best
              , genCount      = generation }
        if predicateOpr gen
          then return gen
          else do
            scaledGs <- scoreScaleOpr gen
            offsprings <- selectionOpr (population `div` 2) crsAndMut scaledGs
            loop (generation + 1) $ runST $ do
              os <- V.unsafeThaw gs
              let writer i (g1, g2) = VM.write os (i*2) g1 >> VM.write os (i*2+1) g2
              V.zipWithM_ writer (V.enumFromTo 0 $ V.length offsprings - 1) offsprings
              V.unsafeFreeze os
  loop 0 initgs
