module Genetic.Selection where

import Genetic.Core

import Control.Monad
import Control.Monad.Random
import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Function

import Prelude hiding (length)
import Data.List (maximumBy)

import Debug.Trace

slcFromStepwise
  :: Monad m
  => (Vector (g, Score) -> m g)
  -> Int -> (g -> g -> m (g, g)) -> Vector (g, Score) -> m (Vector (g, g))
slcFromStepwise sltstep num opr gs = V.replicateM num makeBaby
  where makeBaby = (,) <$> sltstep gs <*> sltstep gs >>= uncurry opr

slcFromStepwiseAsync
  :: MonadBaseControl IO m
  => (Vector (g, Score) -> m g)
  -> Int -> (g -> g -> m (g, g)) -> Vector (g, Score) -> m (Vector (g, g))
slcFromStepwiseAsync sltstep num opr gs =
  runConcurrently $ V.replicateM num makeBaby
  where makeBaby = Concurrently $ (,) <$> sltstep gs <*> sltstep gs >>= uncurry opr

slcStepRouletteWheel :: MonadRandom m => Vector (g, Score) -> m g
slcStepRouletteWheel sgs = do
  slice <- getRandomR (0, fromIntegral $ V.length sgs - 1)
  return $ go slice 0 0
  where
    go slice acc curi =
      if slice < acc + score
        then genome
        else go slice (acc + score) $ curi + 1
      where (genome, score) = sgs V.! curi

slcStepTournament :: MonadRandom m => Int -> Vector (g, Score) -> m g
slcStepTournament gnum sgs = do
  slcs <- take gnum <$> getRandomRs (0, V.length sgs - 1)
  return $ fst $ maximumBy (compare `on` snd) $ map (sgs V.!) slcs
