module Main where
import Data.List.Split (splitOn)
import Numeric (readFloat)
import GHC.Float.RealFracMethods (ceilingFloatInt, floorFloatInt)

intSquare :: Integer -> Integer
intSquare n = go 1 n n
            where go minN maxN sqN= let n = (minN + maxN) `div` 2
                                 in case () of
                                    () | n*n == sqN || n*n < sqN && (n+1) * (n+1) > sqN-> n
                                       | n*n < sqN -> go n maxN sqN
                                       | n*n > sqN -> go minN n sqN
                                       | otherwise -> n

-- number of ways to win a race
-- we need to solve k^2 -nk >= x (k is the number we need to find out).
-- basically, nr of integers between solutions of quadratic equation
raceWinScenarios :: Integer -> Integer -> Integer
raceWinScenarios n x = let root = intSquare (n*n - 4*x)
                       in case () of
                            () | root * root == n*n -4 * x -> if even $ n + root
                                                                then (n+root) `div` 2 - (n-root) `div` 2 -1
                                                                else (n+root) `div` 2 - (n-root) `div` 2
                               | otherwise -> if even $ n + root
                                                 then (n+root) `div` 2 - (n-root) `div` 2 + 1
                                                 else (n+root) `div` 2 - (n-root) `div` 2


main :: IO ()
main = do
  lns <-  lines <$> readFile "data.txt"
  let [times, distances] =  map (\x -> read x::Integer) .  tail . filter (/= "") . splitOn " " <$> lns
  print $ product $ zipWith raceWinScenarios times distances
  -- part 2
  let [time, distance] = (\x-> read x::Integer) . filter (/= ' ') . dropWhile (/=' ') <$> lns
  print $ raceWinScenarios time distance
