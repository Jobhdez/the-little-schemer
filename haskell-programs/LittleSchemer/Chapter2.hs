module LittleSchemer.Chapter2 where

import Data.Char

isString :: String -> Bool
isString str = all isLetter str

isLat :: [String] -> Bool
isLat [] = True
isLat (x:xs) = isString x && rest where
  rest = isLat xs

-- member2
isMember :: [String] -> [Char] -> Bool
isMember [] a = False
isMember (x:xs) a = x == a || isMember (xs) a
