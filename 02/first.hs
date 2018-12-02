import System.IO
import Data.Char
import Data.List

countGroup :: Int -> Int -> String -> Int
countGroup n acc x = if length x == n && acc == 0 then acc + 1 else acc

main = do
    contents <- readFile "input.txt"
    let inputLines = lines contents
    let groupedInput = map (\x -> group (sort x)) inputLines
    print $ (foldl (\acc x -> acc + (foldl (countGroup 2) 0 x)) 0 groupedInput) * (foldl (\acc x -> acc + (foldl (countGroup 3) 0 x)) 0 groupedInput)

