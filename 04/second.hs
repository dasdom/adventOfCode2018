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
    print $ "sleepTimes: " ++ show sleepTimes
    let groupedTimes = groupBy (\x y -> fst x == fst y) $ sortBy (\x y -> compare (fst x) (fst y)) sleepTimes
    let summedTimes = sortBy (\x y -> compare (snd y) (snd x)) $ foldl (\accx x -> accx ++ [foldl (\accy y -> (fst y, snd accy + snd y)) (0,0) x]) [] groupedTimes
    let guardId = fst . head $ summedTimes
    print guardId
    --let dates = getDates input
    let allInfo2 = filter (\x -> not . isGuardId . third $ x) $ zip3 guardIds times what
    let rawSleepTimes2 = foldl combineTimes2 [] allInfo2
    --print rawSleepTimes2
    let sleepTimes2 = foldl (\acc x -> if third x == "k" then acc ++ [(first x, second x)] else acc) [] rawSleepTimes2
    --print sleepTimes2
    let sleepMinutes = foldl (++) [] $ foldl (\accx x -> accx ++ [foldl (\accy y -> accy ++ [(fst x, y)]) [] (snd x)]) [] sleepTimes2
    print sleepMinutes
    let groupedSleepMinutes = last $ last $ sortBy (\xs ys -> compare (length xs) (length ys)) $ group . sort $ sleepMinutes
    print $ fst groupedSleepMinutes * snd groupedSleepMinutes
    


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

combineTimes2 :: [(Int, [Int], String)] -> (Int, Int, String) -> [(Int, [Int], String)]
combineTimes2 acc x = acc ++ [if third x == "asleep" then (first x, [second x], "d") else (first x, [(head . second . last $ acc)..second x - 1], "k")]


