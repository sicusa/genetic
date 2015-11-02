module Main where

import Genetic.Core
import Genetic.Crossover
import Genetic.Mutation
import Genetic.Selection
import Genetic.ScoreScaling

import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Function

import Control.Monad.Random

import System.Random.Shuffle

data City = City
  { cityId  :: Int
  , cityPos :: (Double, Double) }
  deriving (Show)

instance Eq City where
  (==) = (==) `on` cityId

type Genome = Vector Int
type CityMap = Vector City

cityDistance :: City -> City -> Double
cityDistance = posDistance `on` cityPos
  where posDistance (x1, y1) (x2, y2) = sqrt $ (x1 - x2) ** 2 + (y1 - y2) ** 2

tourDistance :: CityMap -> Genome -> Score
tourDistance m g =
    let (lastCity, dis) = V.foldl' go (startCity, 0) g
    in dis + cityDistance lastCity startCity
  where
    go (lastCity, acc) index =
      if lastCity == startCity
        then (currCity, acc)
        else (currCity, acc + cityDistance lastCity currCity)
      where currCity = getCity index
    getCity i = m V.! (g V.! i)
    startCity = getCity 0

main :: IO ()
main = do
  dataLines <- lines <$> readFile "att532.tsp"
  let cityMap = V.fromList $ map (toCity . words) dataLines
      customSettings = GeneticSettings
        { crossoverRate    = 0.75
        , mutationRate     = 0.2
        , maxPopultation   = 50
        , terminationScore = 50000
        , userData         = cityMap}
      customOperators = GeneticOperators
        { crossoverOpr  = crsPartiallyMapped
        , mutationOpr   = mutScramble
        , selectionOpr  = slcTournament 10
        , scoreScaleOpr = sclRanked
        , scoreMarkOpr  = tourDistance cityMap }
      getRandomLists 0 = return []
      getRandomLists i = do
        rl <- shuffleM [0..V.length cityMap - 1]
        (V.fromList rl :) <$> getRandomLists (i - 1)
  genomes <- getRandomLists $ maxPopultation customSettings
  gen <- runGenetic customSettings customOperators $ V.fromList genomes
  print $ genBestScore gen
  where
    toCity (ident:x:y:_) = City (read ident) (read x, read y)
    toCity _ = undefined
