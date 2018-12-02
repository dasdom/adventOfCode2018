import System.IO
import Data.Char
import Data.List
import Data.Maybe

-- This solution is super slow because it produces inits of a cycle of the list with
-- frequency changes. There has to be a better way. I'm totally new to Haskell.
main = do
    contents <- readFile "frequencyChanges.txt"
    let changes = lines contents
    let intChanges = map (\x -> read(filter (/= '+') x) :: Int) changes
    -- This here is super slow.
    let index = findIndex (\x -> if length x > 0 then elem (last x) (init x) else False) $ map (scanl' (+) 0) (inits (cycle intChanges))
    print index
    print $ last $ last $ (take (fromJust index + 1) $ map (scanl (+) 0) (inits (cycle intChanges)))

