import Data.Char ( isDigit )
import Control.Monad.State
import Data.List
import qualified Control.Arrow as A ((&&&))

solution_1 :: [String] -> [Int]
solution_1 mat = snd $ foldl op (mat, []) (numIndices mat)
                where op (accMat, accNums) (i,j) = let (newMat, newNum) = (replaceIntSFrom i j A.&&& parseIntSFrom i j) accMat
                                                   in case newNum of
                                                      (Nothing,_) -> (newMat, accNums)
                                                      (Just x, indicesToCheck)
                                                        | foldr (\ (ix, jx) acc -> ((not.isDigit $ accMat !!ix !!jx) && (mat !! ix !! jx /= '.')) || acc) False indicesToCheck ->(newMat,  x:accNums)
                                                        | otherwise -> (accMat, accNums)

solution_2 :: [String] -> [((Int, Int),Int)]
solution_2 mat = snd $ foldl op (mat, []) (numIndices mat)
                where op (accMat, accNums) (i,j) = let (newMat, newNum) = (replaceIntSFrom i j A.&&& parseIntSFrom i j) accMat
                                                   in case newNum of
                                                      (Nothing,_) -> (newMat, accNums)
                                                      (Just x, indicesToCheck) -> foldr op2 (newMat,accNums) indicesToCheck
                                                            where op2 (ix, jx) (newMat, accNums')
                                                                    | mat !! ix !! jx /= '.' = (newMat,((ix,jx),x) :accNums')
                                                                    | otherwise = (newMat , accNums')

replaceIntSFrom :: Int -> Int -> [String] -> [String]
replaceIntSFrom i j mat = let row = mat !! i
                              newRow = take j row ++ op (drop j row)
                              op [] = []
                              op l@(x:xs) = if isDigit x then '.' : op xs else l
                          in case i of
                            0 -> newRow : drop (i+1) mat
                            139 -> take i mat ++ [newRow]
                            _ -> take i mat ++ newRow : drop (i+1) mat

parseIntSFrom :: Int -> Int -> [String] ->( Maybe Int, [(Int, Int)])
parseIntSFrom i j mat = case foldr op ([], j) intChars of
                          ([], _) -> (Nothing,[])
                          (x, _) -> (Just $ read intChars,x)
                        where
                              intChars = takeWhile isDigit $ drop j $ mat !!i
                              n = length mat - 1
                              op elem (curr_indices, curr_j)
                                      | isDigit elem = (curr_indices `union` indicesToCheck i curr_j n, curr_j+1)
                                      | otherwise = (curr_indices, curr_j+1)

indicesToCheck :: Int -> Int -> Int ->[(Int, Int)]
indicesToCheck i j n = case (i, j) of
                          (0,0) -> [(1,0), (0,1), (1,1)]
                          (0,x)
                            | x==n      -> [(0, n-1), (1, n-1), (1, n)]
                            | otherwise -> [(0, x-1), (1,x-1), (1,x),(0,x+1),(1,x+1)]
                          (x,0)
                            | x == n    -> [(n-1, 0), (n-1, 1), (n,1)]
                            | otherwise -> [(x-1, 0), (x+1,0), (x-1, 1), (x, 1), (x+1,1)]
                          (x,y)
                            | x == n && y == n -> [(n-1,n-1), (n, n-1), (n-1, n)]
                            | x == n -> [(n, y-1), (n-1, y-1), (n-1, y),(n, y+1), (n-1, y+1)]
                            | y == n -> [(x-1, n-1), (x, n-1), (x+1, n-1), (x-1,n), (x+1, n)]
                            | otherwise  -> [(x-1, y-1),  (x, y-1), (x+1,y-1), (x+1, y),(x-1, y), (x-1, y+1), (x, y+1), (x+1, y+1)]

numIndices :: [String] -> [(Int, Int)]
numIndices mat = [(i, j) | i <-[0..length mat - 1], j <- [0..length mat - 1], isDigit (mat !!i !! j) ]

symbolIndices :: [String] -> [(Int, Int)]
symbolIndices mat = sortBy order  [(i, j) | i <-[0..length mat - 1], j <- [0..length mat - 1], not $ isDigit (mat !!i !! j), mat !! i !! j /= '.']

order :: (Int, Int) -> (Int, Int) -> Ordering
order (x1, y1) (x2, y2)
  | y1 < y2 = LT
  | otherwise = GT

duplicateElements ::Show a=> (a -> a -> Bool) -> [a] -> [a]
duplicateElements _ [] = []
duplicateElements p  (x:xs)
  | any (p x) xs = x : duplicateElements p xs
  | otherwise = duplicateElements p xs

main :: IO ()
main = do
        input <- readFile "data.txt"
        print $ sum $solution_1 $ lines input
        let raw = solution_2 $ lines input
        let values = map (\(t, _) ->map snd $ filter (\(t',_) -> t' ==t) raw ) (duplicateElements (\(t,_) (t',_)-> t==t') raw)
        let gearRatioSum = sum $ map product $filter (\x -> length x == 2)  values
        print gearRatioSum -- part 2
