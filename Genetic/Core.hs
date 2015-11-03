{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Genetic.Core where

import GHC.Exts (Constraint)

import Control.Arrow
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST

import Data.Bits ((.&.), (.|.), FiniteBits, Bits)
import qualified Data.Bits as B

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Mutable as VM

import Data.Foldable (foldl', foldr')
import Data.Function (on)

-- for BGenWrap
import Text.Printf (PrintfArg)
import Test.QuickCheck (Arbitrary)
import Data.Ix (Ix)
import Data.Data (Data)
import Foreign.Storable (Storable)

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

newtype BGenWrap b = BGen b
  deriving (Bounded, Enum, Eq, Integral, Num, Ord, Read, Real, Show
           ,FiniteBits, Bits, PrintfArg, Arbitrary, Data, Storable)

instance FiniteBits b => GenomeBase (BGenWrap b) where
  type Gene (BGenWrap b) = Bool

  {-# INLINE length #-}
  length = B.finiteBitSize

  fromList f = snd $ foldl' go (0, B.zeroBits) f
    where
      go (num, res) element
        | element   = (num + 1, B.bit num .|. res)
        | otherwise = (num + 1, res)

  {-# INLINE getG #-}
  getG i b = B.testBit b i

{-# INLINE bitSet #-}
bitSet :: FiniteBits b => Int -> Bool -> b -> b
bitSet i True = flip B.setBit i
bitSet i _    = flip B.clearBit i

bitPairs :: FiniteBits b => [(Int, Bool)] -> b -> b
bitPairs l b = foldl' (\a (i, v) -> bitSet i v a) b l

instance FiniteBits b => PermutationGenome (BGenWrap b) where
  {-# INLINE swap #-}
  swap i1 i2 b =
    let v1 = B.testBit b i1
        v2 = B.testBit b i2
    in bitSet i1 v2 . bitSet i2 v1 $ b

  offset i o b =
    if adj_o == 0
      then b
      else bitSet (i + adj_o) (B.testBit b i) $ repRes .|. (b .&. B.complement mask)
    where
      lasti = length b - 1
      adj_o = max (-i) . min (lasti - i) $ o
      (ib, ie) | adj_o > 0 = (i, i + adj_o)
               | otherwise = (i + adj_o, i)
      emptyMask = B.complement B.zeroBits
      mask = uncurry (.&.) (flip B.shiftR (lasti - ie) &&& flip B.shiftL ib $ emptyMask)
      repRes = B.shift b (if adj_o > 0 then -1 else 1) .&. mask

  offsetRange (beg, end) o b =
    if adj_o == 0
      then b
      else (B.shift b s .&. transferMask) .|. (B.shift b o .&. replacedMask) .|. remain
    where
      s = if adj_o > 0 then -rlen else rlen
      rlen = end - beg + 1
      lasti = length b - 1
      adj_o = max (-beg) . min (lasti - end) $ o
      (rbeg, rend) = (beg + adj_o, end + adj_o)
      (ib, ie) | adj_o > 0 = (beg, rbeg - 1)
               | otherwise = (rend + 1, end)
      emptyMask = B.complement B.zeroBits
      makeMask r l = uncurry (.&.) (flip B.shiftR (lasti - r) &&& flip B.shiftL l $ emptyMask)
      transferMask = makeMask ie   ib
      replacedMask = makeMask rend rbeg
      remain = b .&. B.complement (transferMask .|. replacedMask)

instance FiniteBits b => FreeGenome (BGenWrap b) where
  {-# INLINE setG #-}
  setG i bitvalue b
    | bitvalue  = B.setBit b i
    | otherwise = B.clearBit b i

  swapRangeBetween (beg, end) b1 b2 =
    (b1 .&. rmask .|. b2range, b2 .&. rmask .|. b1range)
    where
      lasti = length b1 - 1
      emptyMask = B.complement B.zeroBits
      mask = uncurry (.&.) (flip B.shiftR (lasti - end) &&& flip B.shiftL beg $ emptyMask)
      rmask = B.complement mask
      b1range = mask .&. b1
      b2range = mask .&. b2

instance FiniteBits b => BinaryGenome (BGenWrap b) where
  {-# INLINE shift #-}
  shift = flip B.shift
  {-# INLINE gand #-}
  gand = (.&.)
  {-# INLINE gor #-}
  gor  = (.|.)
  {-# INLINE gxor #-}
  gxor = B.xor

instance GenomeBase (Vector a) where
  type Gene (Vector a) = a

  {-# INLINE length #-}
  length = V.length

  {-# INLINE fromList #-}
  fromList = V.fromList

  {-# INLINE toList #-}
  toList = V.toList

  {-# INLINE getG #-}
  getG = flip (V.!)

instance PermutationGenome (Vector a) where
  {-# INLINE swap #-}
  swap i1 i2 g = runST $ do
    mv <- V.thaw g
    VM.swap mv i1 i2
    V.unsafeFreeze mv

  {-# INLINE swapMulti #-}
  swapMulti ips g = runST $ do
    mv <- V.thaw g
    mapM_ (uncurry $ VM.swap mv) ips
    V.unsafeFreeze mv

  offset _ 0 g = g
  offset i o g = runST $ do
    mv <- V.thaw g
    if adj_o > 0
      then mapM_ (\x -> VM.write mv (x - 1) $ g V.! x) [i+1..fpos]
      else mapM_ (\x -> VM.write mv (x + 1) $ g V.! x) [fpos..i-1]
    VM.write mv fpos $ g V.! i
    V.unsafeFreeze mv
    where
      lasti = length g - 1
      adj_o = max (-i) . min (lasti - i) $ o
      fpos = i + adj_o

  offsetRange (beg, end) o g = runST $ do
    mv <- V.thaw g
    if adj_o > 0
      then mapM_ (\x -> VM.write mv (x - diff) $ g V.! x) [end+1..end+adj_o]
      else mapM_ (\x -> VM.write mv (x + diff) $ g V.! x) [beg-1..beg+adj_o]
    mapM_ (\x -> VM.write mv (x + adj_o) $ g V.! x) [beg..end]
    V.unsafeFreeze mv
    where
      lasti = length g - 1
      adj_o = max (-beg) . min (lasti - end) $ o
      diff = end - beg

instance FreeGenome (Vector a) where
  {-# INLINE setG #-}
  setG i v g = runST $ do
    mv <- V.thaw g
    VM.write mv i v
    V.unsafeFreeze mv

  swapBetween i1 i2 g1 g2 = runST $ do
    mv1 <- V.thaw g1
    mv2 <- V.thaw g2
    VM.write mv1 i1 v2
    VM.write mv2 i2 v1
    (,) <$> V.unsafeFreeze mv1 <*> V.unsafeFreeze mv2
    where
      v1 = g1 V.! i1
      v2 = g2 V.! i2

  swapRangeBetween (beg, end) g1 g2 = runST $ do
    mv1 <- V.thaw g1
    mv2 <- V.thaw g2
    let {
      swap i = do
        let v1 = g1 V.! i
            v2 = g2 V.! i
        VM.write mv1 i v2
        VM.write mv2 i v1
    }
    mapM_ swap [beg..end]
    (,) <$> V.unsafeFreeze mv1 <*> V.unsafeFreeze mv2

type Probability = Double
type Score = Double

data Generation g = Generation
  { genGenomes    :: Vector g
  , genScores     :: Vector Score
  , genTotalScore :: Score
  , genBestScore  :: Score
  , genNum        :: Int }

data GeneticSettings u = GeneticSettings
  { crossoverProb    :: Probability
  , mutationProb     :: Probability
  , maxPopultation   :: Int
  , terminationScore :: Score
  , userData         :: u }

data GeneticOperators g = GeneticOperators
  { scoreMarkOpr  :: g -> Score
  , scoreScaleOpr :: Generation g -> Vector (g, Score)
  , selectionOpr  :: forall m. MonadRandom m => Vector (g, Score) -> m g
  , crossoverOpr  :: forall m. MonadRandom m => g -> g -> m (g, g)
  , mutationOpr   :: forall m. MonadRandom m => g -> m g }

{-# INLINE probEvent #-}
probEvent :: (MonadRandom m) => Probability -> m g -> m g -> m g
probEvent r def m = do
  rf <- getRandomR (0, 1)
  if rf > r then def else m

{-# INLINE probValue #-}
probValue :: (MonadRandom m) => Probability -> g -> g -> m g
probValue r def slt = do
  rf <- getRandomR (0, 1)
  return $ if rf > r then def else slt

mapPairM :: (Monad m) => (a -> m b) -> (a, a) -> m (b, b)
mapPairM f (a, b) = (,) <$> f a <*> f b

concatTuples :: Vector (a, a) -> Vector a
concatTuples v = let (l, r) = V.unzip v in V.concat [l, r]

runGenetic :: (GenomeBase g, MonadRandom m) => GeneticSettings u -> GeneticOperators g -> Vector g -> m (Generation g)
runGenetic (GeneticSettings{..}) (GeneticOperators{..}) = loop 0
  where
    loop generation gs =
      if totalScore > terminationScore
        then return gen
        else do
          offsprings <- concatTuples <$> V.replicateM (maxPopultation `div` 2) makeBaby
          loop (generation + 1) offsprings
      where
        makeBaby = do
          father <- selectionOpr scaledGs
          mother <- selectionOpr scaledGs
          probCrossoverOpr father mother >>= mapPairM probMutationOpr
        scaledGs = scoreScaleOpr gen
        gen = Generation
          { genGenomes    = gs
          , genScores     = scores
          , genTotalScore = totalScore
          , genBestScore  = bestScore
          , genNum        = generation }
        scores = fmap scoreMarkOpr gs
        totalScore = V.sum scores
        bestScore  = V.maximum scores
    probCrossoverOpr g1 g2 = probEvent crossoverProb (return (g1, g2)) $ crossoverOpr g1 g2
    probMutationOpr g = probEvent mutationProb (return g) $ mutationOpr g
