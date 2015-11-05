{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Genetic.Instance where

import Genetic.Core

import qualified Data.Vector as V
import Data.Vector (Vector)
import qualified Data.Vector.Mutable as VM

import Control.Arrow
import Control.Monad
import Control.Monad.ST

import Data.Bits ((.&.), (.|.), FiniteBits, Bits)
import qualified Data.Bits as B

import Data.Foldable (foldl', foldr')
import Data.Function (on)

-- for BGenWrap
import Text.Printf (PrintfArg)
import Test.QuickCheck (Arbitrary)
import Data.Ix (Ix)
import Data.Data (Data)
import Foreign.Storable (Storable)

import Prelude hiding (length)

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
