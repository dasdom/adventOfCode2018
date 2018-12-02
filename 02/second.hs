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
    let resultArray = foldl (\acc x -> acc ++ [head x]) [] (filter (\x -> length x > 0) (map (\x -> (foldl (appendIfDiffIsOne x) [] input )) input))
    print $ intersect (head resultArray) (last resultArray)
