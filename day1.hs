import Data.Char (isDigit)
import Control.Arrow ((&&&))
-- day one in aoc 2023


solution :: [String] -> Int
solution =sum . map ((\x -> read x::Int). (\(a,b) -> [a, b]) . (head &&& last) . filter isDigit)
