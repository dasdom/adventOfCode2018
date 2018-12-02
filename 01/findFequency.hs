import System.IO
import Data.Char

main = do
    contents <- readFile "frequencyChanges.txt"
    let changes = lines contents
    print $ foldl (\acc x -> acc + read(filter (/= '+') x)) 0 changes
