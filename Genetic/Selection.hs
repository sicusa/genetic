module Genetic.Selection where

import Genetic.Core

import Control.Monad
import Control.Monad.Random

import qualified Data.Vector as V
import Data.Function

import Prelude hiding (length)
import Data.List (maximumBy)

slcRouletteWheel :: (MonadRandom m) => GeneticSettings u -> ScoredGenomes g -> m g
slcRouletteWheel _ sgs = do
  slice <- getRandomR (0, totalScore)
  return $ go slice 0 0
  where
    genomes = sgenomes sgs
    totalScore = stotal sgs
    go slice acc curi =
      if slice < acc + score
        then genome
        else go slice (acc + score) $ curi + 1
      where (genome, score) = genomes V.! curi

slcTournament :: (MonadRandom m) => Int -> GeneticSettings u -> ScoredGenomes g -> m g
slcTournament gnum _ sgs = do
  slcs <- take gnum <$> getRandomRs (0, V.length genomes - 1)
  return $ fst $ maximumBy (compare `on` snd) $ map (genomes V.!) slcs
  where genomes = sgenomes sgs
