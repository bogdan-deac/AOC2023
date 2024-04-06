import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PC
import Data.List.Split ( splitOn )
import Data.Char (isSpace, isDigit)

import Control.Monad (join)
import qualified Control.Arrow as A ((***), (&&&), Arrow)



data Game = Game
  { red :: Int
  , green :: Int
  , blue :: Int
}

gameSplit :: String -> [Game]
gameSplit input = [ foldl updateGame (Game 0 0 0) (splitOn "," row) | row <- splitOn ";" input]


updateGame :: Game -> String -> Game
updateGame game update = case words update of
                          [num, field] -> let numI = read num :: Int in case field of
                               "red" -> game {red = numI}
                               "green" -> game {green = numI}
                               "blue" -> game {blue = numI}
                               _ -> game
                          _ -> game

isGameValid :: Game -> Bool
isGameValid game = red game <= 12 && green game <= 13 && blue game <= 14

main :: IO ()
main = do
        dataVals <- readFile "data.txt"
        let lns = drop 5 <$> lines dataVals
        let indexToGames = [(read (takeWhile isDigit line):: Int, gameSplit $ tail $ dropWhile (not.isSpace) line) |line <- lns]
        let validGames = filter (\(i, games) -> all isGameValid games) indexToGames
        print $ sum $ map fst validGames -- part 1
        let maxBalls = map ((maximum . map red A.&&& maximum. map green A.&&& maximum. map blue). snd ) indexToGames
        let powers = map (\(a,(b,c)) -> a*b*c) maxBalls
        print $ sum powers



