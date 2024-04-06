import Data.Char (isDigit)
import Control.Arrow ((&&&))
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.Char as PC
import Control.Applicative ((<|>))
import Data.Functor (($>))

-- solution :: [String] -> Int
solution = sum . map ((\x -> read x::Int). (\(a,b) -> [a, b]) . (head &&& last))


parser :: Parsec.Parsec String () [String]
parser =  Parsec.many1 ( Parsec.choice
                        (Parsec.try . Parsec.string <$>
                            ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                            "zero", "one", "two", "three", "four", "five", "six", "seven", "eight","nine"]
                        ) <|> PC.letter $> "")

wordToDigit :: String -> String
wordToDigit "zero" = "0"
wordToDigit "one" = "1"
wordToDigit "two" = "2"
wordToDigit "three" = "3"
wordToDigit "four" = "4"
wordToDigit "five" = "5"
wordToDigit "six" = "6"
wordToDigit "seven" = "7"
wordToDigit "eight" = "8"
wordToDigit "nine" = "9"
wordToDigit str = str

main = do
        x <- readFile "data.txt"
        let ls = lines x
        let edigits = [Parsec.parse parser "" l| l <- ls]
        let digits = [head.wordToDigit <$> filter (/="") a | Right a <- edigits, not (null a)]
        print $ solution digits
