import Data.List
import Data.Maybe

import System.IO
import Data.Char

main = do
    contents <- getContents
    let input = lines contents
    -- print input
    let tuple = zip4 (foldl appendXCoor [] input) (foldl appendYCoor [] input) (foldl appendWidth [] input) (foldl appendHeight [] input)
    print tuple
    let allSquares = foldl (\accx x -> accx ++ foldl (\accy y -> if x /= y then accy ++ overlap x y else accy) [] tuple) [] tuple
    print $ length allSquares
    print $ length . nub $ allSquares


splitAtChar :: Int -> Char -> String -> (String, String)
splitAtChar i s x = splitAt (fromJust . elemIndex s $ words x !! i) $ words x !! i

splitAtComma :: String -> (String, String)
splitAtComma = splitAtChar 2 ','

splitAtX :: String -> (String, String)
splitAtX = splitAtChar 3 'x'

dropFirst :: [a] -> [a]
dropFirst x = drop 1 x

dropLast :: [a] -> [a]
dropLast x = take (length x - 1) x

readXCoor :: (String, String) -> Int
readXCoor x = read (fst x)

readYCoor :: (String, String) -> Int
readYCoor x = read (dropFirst . dropLast . snd $ x)

readWidth :: (String, String) -> Int
readWidth x = read (fst x)

readHeight :: (String, String) -> Int
readHeight x = read (dropFirst . snd $ x)

appendXCoor :: [Int] -> String -> [Int]
appendXCoor acc x = acc ++ [(readXCoor . splitAtComma $ x)]

appendYCoor :: [Int] -> String -> [Int]
appendYCoor acc x = acc ++ [(readYCoor . splitAtComma $ x)]

appendWidth :: [Int] -> String -> [Int]
appendWidth acc x = acc ++ [(readWidth . splitAtX $ x)]

appendHeight :: [Int] -> String -> [Int]
appendHeight acc x = acc ++ [(readHeight . splitAtX $ x)]

first :: (a, b, c, d) -> a
first (w, _, _, _) = w

second :: (a, b, c, d) -> b
second (_, x, _, _) = x

third :: (a, b, c, d) -> c
third (_, _, y, _) = y

fourth :: (a, b, c, d) -> d
fourth (_, _, _, z) = z

isXOverlap :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
isXOverlap x y = (if first x <= first y then first x + third x - first y else first y + third y - first x) > 0

isYOverlap :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Bool
isYOverlap x y = (if second x <= second y then second x + fourth x - second y else second y + fourth y - second x) > 0

upperLeft :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int)
upperLeft x y = (if first x <= first y then first y else first x, if second x <= second y then second y else second x)

lowerRight :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> (Int, Int)
lowerRight x y = (min (first x + third x - 1) (first y + third y - 1), min (second x + fourth x - 1) (second y + fourth y - 1))

overlap :: (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> [(Int, Int)]
overlap x y = if isXOverlap x y && isYOverlap x y then [(v,w) | v <- [(fst $ upperLeft x y)..(fst $ lowerRight x y)], w <- [(snd $ upperLeft x y)..(snd $ lowerRight x y)]] else []


