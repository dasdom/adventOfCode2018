
module Dropper
( dropFirst
, dropLast
) where

dropFirst :: [a] -> [a]
dropFirst x = drop 1 x

dropLast :: [a] -> [a]
dropLast x = take (length x - 1) x
