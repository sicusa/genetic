module Main where

import Genetic.Core

settings :: GeneticSettings ()
settings = GeneticSettings
  { crossoverRate  = 0.75
  , mutationRate   = 0.2
  , maxPopultation = 50 }

main :: IO ()
main = putStrLn "Hello world"
