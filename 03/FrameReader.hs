
module FrameReader
( readXCoor
, readYCoor
, readWidth
, readHeight
, appendXCoor
, appendYCoor
, appendWidth
, appendHeight
, appendId
, splitAtChar
, splitAtComma
, splitAtX
) where

import Data.List
import Data.Maybe
import Dropper

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

appendId :: [Int] -> String -> [Int]
appendId acc x = acc ++ [read . dropFirst $ words x !! 0]

splitAtChar :: Int -> Char -> String -> (String, String)
splitAtChar i s x = splitAt (fromJust . elemIndex s $ words x !! i) $ words x !! i

splitAtComma :: String -> (String, String)
splitAtComma = splitAtChar 2 ','

splitAtX :: String -> (String, String)
splitAtX = splitAtChar 3 'x'
