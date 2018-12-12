import System.IO
import Dropper
import Data.List

main = do
    contents <- getContents
    let input = sort . lines $ contents
    let what = getWhat input
    let times = getTimes input
    let guardIds = getGuardIds input
    let allInfo = filter (\x -> not . isGuardId . third $ x) $ zip3 guardIds times what
    let rawSleepTimes = foldl combineTimes [] allInfo
    let sleepTimes = foldl (\acc x -> if third x == "k" then acc ++ [(first x, second x)] else acc) [] rawSleepTimes
    let groupedTimes = groupBy (\x y -> fst x == fst y) $ sortBy (\x y -> compare (fst x) (fst y)) sleepTimes
    let summedTimes = sortBy (\x y -> compare (snd y) (snd x)) $ foldl (\accx x -> accx ++ [foldl (\accy y -> (fst y, snd accy + snd y)) (0,0) x]) [] groupedTimes
    let guardId = fst . head $ summedTimes
    print guardId
    let dates = getDates input
    let allInfo2= filter (\x -> not . isGuardId . third4 $ x) $ zip4 guardIds times what dates
    let rawSleepTimes2 = foldl combineTimes2 [] $ filter (\x -> first4 x == guardId) allInfo2
    let sleepTimes2 = foldl (\acc x -> if third4 x == "k" then acc ++ [(first4 x, second4 x, dropFirst . fourth4 $ x)] else acc) [] rawSleepTimes2
    let groupedTimes2 = groupBy (\x y -> third x == third y) sleepTimes2
    let summedTimes2 = foldl (\accx x -> accx ++ [foldl (\accy y -> accy ++ second y) [] x]) [] groupedTimes2
    let minute = fst . last . sortOn snd $ foldl (\accx x -> accx ++ [foldl (\accy y -> if elem x y then (x, snd accy + 1) else (x, snd accy)) (0,0) summedTimes2]) [] [1..59]
    print minute
    print $ minute * guardId
    


isGuardId :: String -> Bool
isGuardId x = (head $ x) == '#'

guardId :: String -> Int
guardId x = read . dropFirst $ x

minutes :: String -> Int
minutes x = read . drop 3 $ x

first :: (a, b, c) -> a
first (k, _, _) = k

second :: (a, b, c) -> b
second (_, l, _) = l

third :: (a, b, c) -> c
third (_, _, m) = m

first4 :: (a, b, c, d) -> a
first4 (k, _, _, _) = k

second4 :: (a, b, c, d) -> b
second4 (_, l, _, _) = l

third4 :: (a, b, c, d) -> c
third4 (_, _, m, _) = m

fourth4 :: (a, b, c, d) -> d
fourth4 (_, _, _, n) = n

getDates :: [String] -> [String]
getDates xs = foldl (\acc x -> acc ++ [date x]) [] xs
    where date x = words x !! 0

getWhat :: [String] -> [String]
getWhat xs = foldl (\acc x -> acc ++ [what x]) [] xs
    where what x = words x !! 3

getTimes :: [String] -> [Int]
getTimes xs = foldl (\acc x -> acc ++ [time x]) [] xs
    where time x = minutes . dropLast $ words x !! 1

getGuardIds :: [String] -> [Int]
getGuardIds xs = foldl (\acc x -> if isGuardId x then acc ++ [guardId x] else acc ++ [last acc]) [] $ getWhat xs

startTime :: (Int, Int, String) -> (Int, Int, String)
startTime x = (first x, second x, "d")

stopTime :: (Int, Int, String) -> [(Int, Int, String)] -> (Int, Int, String)
stopTime x acc = (first x, second x - (second . last $ acc), "k")

combineTimes :: [(Int, Int, String)] -> (Int, Int, String) -> [(Int, Int, String)]
combineTimes acc x = acc ++ [if third x == "asleep" then startTime x else stopTime x acc]

combineTimes2 :: [(Int, [Int], String, String)] -> (Int, Int, String, String) -> [(Int, [Int], String, String)]
combineTimes2 acc x = acc ++ [if third4 x == "asleep" then (first4 x, [second4 x], "d", fourth4 x) else (first4 x, [(head . second4 . last $ acc)..second4 x - 1], "k", fourth4 x)]


