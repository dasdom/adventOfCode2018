import System.IO
import Data.Char
import Data.List
import Data.Maybe

main = do
    contents <- readFile "frequencyChanges.txt"
    let changes = lines contents
    let intChanges = map (\x -> read(filter (/= '+') x) :: Int) changes
    let index = findIndex (\x -> if length x > 0 then elem (last x) (init x) else False) $ map (scanl' (+) 0) (inits (cycle intChanges))
    print index
    print $ last $ last $ (take (fromJust index + 1) $ map (scanl (+) 0) (inits (cycle intChanges)))

