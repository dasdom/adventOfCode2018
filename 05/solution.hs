import System.IO
import Data.Char
import Data.List

main = do
    contents <- getContents
    let input = head 
                . lines $ contents
    print $ "1) " ++ (show 
                      . units 
                      $ input)
    print $ "2) " ++ (show 
                      . head 
                      . sort 
                      $ foldl (addUnitsForFilter input) [] ['a'..'z'])


--------------------------------------------------------------
-- addUnitsForFilter: filter for char (lower and upper case) and calculate units
--                    for filteres strings
addUnitsForFilter :: String -> [Int] -> Char -> [Int]
addUnitsForFilter input acc x = acc ++ [units . filterUnitType x $ input]

-- units: number of units after all reactions
units :: String -> Int
units xs = length 
           . last 
           . takeWhileDifferent 
           . iterate react $ xs

-- filterUnitType: filter upper and lower case of a given char
filterUnitType :: Char -> String -> String
filterUnitType x xs = filter (/= toLower x) 
                      . filter (/= toUpper x) $ xs

-- react: remove lower case and upper case of the same char 
--        if they happen to be next to each other.
react :: String -> String
react (x:y:xs)
    | null xs = x:y:[]
    | x == y = x:react (y:xs)
    | toUpper x == y = react xs
    | x == toUpper y = react xs
    | otherwise = x:react (y:xs)
react (x:xs)
    | null xs = x:[]

-- takeWhileDifferent: take elements from the list as long as the 
--                     items are changing
takeWhileDifferent :: [String] -> [String]
takeWhileDifferent (x:y:xs)
    | x == y = x:y:[]
    | otherwise = x:y:(takeWhileDifferent xs)


