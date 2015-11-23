{-# LANGUAGE FlexibleContexts #-}

module Main where

import Genetic.Core
import Genetic.Crossover
import Genetic.Mutation
import Genetic.Selection
import Genetic.ScoreScaling
import Genetic.Instance

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Function

import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Control.Concurrent.Async.Lifted

import System.Random.Shuffle
import Debug.Trace

data City = City
  { cityId  :: Int
  , cityPos :: (Double, Double) }
  deriving (Show)

instance Eq City where
  (==) = (==) `on` cityId

type Genome = Vector Int
type CityMap = Vector City
type Settings = GeneticSettings CityMap

cityDistance :: City -> City -> Double
cityDistance = posDistance `on` cityPos
  where posDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

tourDistance :: Monad m => CityMap -> Genome -> m Score
tourDistance m g =
    let (lastCity, dis) = V.foldl' go (startCity, 0) g
    in return $ dis + cityDistance lastCity startCity
  where
    go (lastCity, acc) index =
      if lastCity == startCity
        then (currCity, acc)
        else (currCity, acc + cityDistance lastCity currCity)
      where currCity = getCity index
    getCity i = m V.! (g V.! i)
    startCity = getCity 0

scoreMarker
  :: (MonadReader Settings m, MonadIO m)
  => Vector Genome -> m (Vector Score)
scoreMarker gs = do
  cityMap <- asks gsUserData
  distances <- V.mapM (tourDistance cityMap) gs
  let longestDis  = maximum distances
      shortestDis = minimum distances
  liftIO $ print shortestDis
  return $ V.map (\g -> longestDis - g + 1) distances

scoreMarker2
  :: (MonadReader Settings m, MonadIO m)
  => Vector Genome -> m (Vector Score)
scoreMarker2 gs = do
  cityMap <- asks gsUserData
  distances <- V.mapM (tourDistance cityMap) gs
  let shortestDis = minimum distances
  liftIO $ print shortestDis
  return $ V.map (\g -> 1 / g) distances

main :: IO ()
main = do
  dataLines <- lines <$> readFile "/home/xikusa/att532.tsp"
  let cityMap = V.fromList $ map (toCity . words) dataLines
      customSettings = GeneticSettings
        { gsCrossoverProb    = 0.75
        , gsMutationProb     = 0.2
        , gsPopulation       = 50
        , gsUserData         = cityMap }
      customOperators = GeneticOperators
        { crossoverOpr  = crsQuickPartiallyMapped 10
        , mutationOpr   = mutSwap
        , selectionOpr  = slcFromStepwiseAsync $ slcStepTournament 10
        , scoreScaleOpr = sclRanked
        , scoreMarkOpr  = scoreMarker
        , predicateOpr  = const False }
      getRandomLists 0 = return []
      getRandomLists i = do
        rl <- shuffleM [0..V.length cityMap - 1]
        (V.fromList rl :) <$> getRandomLists (i - 1)
  genomes <- getRandomLists $ gsPopulation customSettings
  gen <- runGenetic customSettings customOperators $ V.fromList genomes
  print $ genBestGenome gen
  where
    toCity (ident:x:y:_) = City (read ident) (read x, read y)
    toCity _ = undefined
