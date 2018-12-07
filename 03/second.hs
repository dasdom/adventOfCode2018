import Data.List
--import Data.Maybe

import System.IO
import Data.Char

import Dropper
import FrameReader

main = do
    contents <- getContents
    let input = lines contents
    print input
    let tuple = zip5 (foldl appendXCoor [] input) (foldl appendYCoor [] input) (foldl appendWidth [] input) (foldl appendHeight [] input) (foldl appendId [] input)
    print tuple
    let overlapping = foldl (\accx x -> accx ++ foldl (\accy y -> if x /= y && isXOverlap x y && isYOverlap x y then accy ++ [x] else accy) [] tuple) [] tuple
    print overlapping
    print $ tuple \\ overlapping


first :: (a, b, c, d, e) -> a
first (k, _, _, _, _) = k

second :: (a, b, c, d, e) -> b
second (_, l, _, _, _) = l

third :: (a, b, c, d, e) -> c
third (_, _, m, _, _) = m

fourth :: (a, b, c, d, e) -> d
fourth (_, _, _, n, _) = n

fifth :: (a, b, c, d, e) -> e
fifth (_, _, _, _, o) = o

isXOverlap :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Bool
isXOverlap x y = (if first x <= first y then first x + third x - first y else first y + third y - first x) > 0

isYOverlap :: (Int, Int, Int, Int, Int) -> (Int, Int, Int, Int, Int) -> Bool
isYOverlap x y = (if second x <= second y then second x + fourth x - second y else second y + fourth y - second x) > 0

