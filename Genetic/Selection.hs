module Genetic.Selection where

import Genetic.Core

import Control.Monad
import Control.Monad.Random

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Function

import Prelude hiding (length)
import Data.List (maximumBy)

slcRouletteWheel :: (MonadRandom m) => Vector (g, Score) -> m g
slcRouletteWheel sgs = do
  slice <- getRandomR (0, fromIntegral $ V.length sgs - 1)
  return $ go slice 0 0
  where
    go slice acc curi =
      if slice < acc + score
        then genome
        else go slice (acc + score) $ curi + 1
      where (genome, score) = sgs V.! curi

slcTournament :: (MonadRandom m) => Int -> Vector (g, Score) -> m g
slcTournament gnum sgs = do
  slcs <- take gnum <$> getRandomRs (0, V.length sgs - 1)
  return $ fst $ maximumBy (compare `on` snd) $ map (sgs V.!) slcs
