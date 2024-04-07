{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Data.Map as M
import Control.Arrow ((&&&))
import qualified Data.Vector as V
import Data.Foldable (foldl')
import Data.Bifunctor(second, bimap)
import Control.Monad(liftM)

modifyAt :: Int -> (a->a) -> [a] -> [a]
modifyAt i f arr =  let (init, h:t) = splitAt i arr
                    in if i >= length arr then arr else init ++ (f h: t)

winningNumbersToMap :: [Int] -> M.Map Int ()
winningNumbersToMap = foldr (`M.insert` ()) M.empty

getCorrectCount :: M.Map Int () -> [Int]-> Int
getCorrectCount map = foldr (\num -> (+) (if M.member num map then 1 else 0)) 0

viableScores :: [Int]
viableScores = 0 : 1 : map (*2) (tail viableScores)

getScore :: [Int] -> [Int] -> Int
getScore winners  = (!!) viableScores . getCorrectCount (winningNumbersToMap winners)

winnerCount :: [Int] -> [Int] -> Int
winnerCount = getCorrectCount . winningNumbersToMap

propagateStep :: V.Vector Int -> V.Vector Int -> Int -> V.Vector Int
propagateStep  weights vals idx =
  foldl' (\newArr step -> let pos = idx + step in
          updateAt newArr pos ( newArr V.! idx + newArr V.! pos) )
          vals
          [1.. weights V.! idx]

-- yolo unsafe stuff, cleaner interface
updateAt :: V.Vector a -> Int -> a -> V.Vector a
updateAt v i x = V.unsafeUpd v [(i,x)]

-- Some slight optimizations - using vectors for heavy mutation tasks.
propagate :: [Int] -> [Int] -> [Int]
propagate weights vals = V.toList $ foldl' (propagateStep vWeights) vvals [0..V.length vvals - 1]
                          where vvals = V.fromList vals
                                vWeights = V.fromList weights

main :: IO ()
main = do
        fileData <- map (words . tail . dropWhile (/= ':')) . lines <$> readFile "data.txt"
        let op = (<$>).(<$>)
        let (winners, numbers) = bimap (op read) (op read. fmap tail) $ unzip  ( span (/= "|") <$> fileData)
        print $ sum $ zipWith getScore winners numbers -- part 1
        let weights =  zipWith winnerCount winners numbers
        print $ sum $ propagate weights [1 | _ <- weights ] --part 2
