import System.IO
import Data.List

appendIfDifferent :: (Eq a) => [(a, a)] -> (a, a) -> [(a, a)]
appendIfDifferent xs x = if fst x /= snd x then xs ++ [x] else xs

numberOfDifferences :: String -> String -> Int
numberOfDifferences x y = length $ foldl appendIfDifferent [] (zip x y)

appendIfDiffIsOne :: String -> [String] -> String -> [String]
appendIfDiffIsOne x acc y = if numberOfDifferences x y == 1 then acc ++ [y] else acc


main = do
    contents <- readFile "input.txt"
    let input = lines contents
    let resultArray = foldl (\acc x -> if length (foldl (appendIfDiffIsOne x) [] input) > 0 then acc ++ [x] else acc) [] input
    --print resultArray
    --print $ intersect (head resultArray) (last resultArray)
    print $ foldl (\acc x -> if fst x == snd x then acc ++ [fst x] else acc) [] $ zip (head resultArray) (last resultArray)
