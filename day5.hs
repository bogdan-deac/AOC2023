{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as M
import Data.List.Split (linesBy, splitOn)


-- for type safety reasons
type Seed = Int
type Soil = Int
type Fertilizer = Int
type Water = Int
type Light = Int
type Temperature = Int
type Humidity = Int
type Location = Int


mLookup ::( Ord a, Num a )=> [(a,a,a)] -> a -> a
mLookup [] x = x
mLookup ((lowV, low, range):ms) x
  | x >= low && x < low + range = lowV + x - low
  | otherwise = mLookup ms x

readVals :: (Read a, Num a) => [String] -> [(a, a, a)]
readVals  =  map ((\[x,y,z]-> (read x, read y, read z) ).words) . tail

listPairs :: [a] ->  [[a]]
listPairs [] = []
listPairs (x:y:xs) = [x,y]: listPairs xs
listPairs _ = error "Nope"

main :: IO ()
main = do
          chunks <- splitOn "\n\n" <$> readFile "data.txt"
          let seeds = map read $  tail $ words $ head chunks
          let maps =  readVals . lines <$> tail chunks
          print maps
          print $ minimum  [foldl (flip mLookup) seed maps | seed <- seeds ]
          let seedsPart2 = do
                              pair <- listPairs seeds
                              [head pair .. (pair!!1) - 1]
          print $ length seedsPart2
          return ()
