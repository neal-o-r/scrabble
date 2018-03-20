module Utils
( range
, add_arr
, mul_arr
, mul_num
, add_num
, replaceAtIndex
, insertChar_in
, insertChar
, regex_indices
, color_print
, up
, low
, subanagrams)
where

import System.IO
import Data.Char
import Data.List
import Data.Foldable
import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base

range step start end 
    | step > 0 = takeWhile (<=end) $ iterate (+step) start
    | otherwise = takeWhile (>=end) $ iterate (+step) start
add_arr a b = zipWith (+) a b
mul_arr a b = zipWith (*) a b
mul_num a b = map (* a) b
add_num a b = map (+ a) b

up s = map toUpper s
low s = map toLower s

shuffle_pairs :: [t] -> [t] -> [[t]]
shuffle_pairs xs [] = [xs]
shuffle_pairs [] ys = [ys]
shuffle_pairs (x:xs) (y:ys) =
      map (x:) (shuffle_pairs xs (y:ys)) ++ map (y:) (shuffle_pairs (x:xs) ys)

anagrams :: [Char] -> [[Char]]
anagrams = foldrM shuffle_pairs "" . group . sort
subanagrams :: [Char] -> [[Char]]
subanagrams = foldrM f "" . map tails . group . sort where
            f is j = is >>= flip shuffle_pairs j



replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- put in a char at a place
insertChar_in :: String -> (Int, Char) -> String
insertChar_in board state =
        let loc = fst state
            c = snd state
         in replaceAtIndex loc c board

-- put in a char at a place, and a blank space after
insertChar :: String -> (Int, Char) -> String
insertChar board state =
        let loc = fst state
            c = snd state
         in replaceAtIndex (succ loc) ' ' (replaceAtIndex loc c board)

regex_indices :: Regex -> String -> [Int]
regex_indices reg str = [ i | (i,x) <- zip [0..] str, (matchTest reg [x])]

-- print something with color,
-- give a predicate on which color changes
-- color to change to
color_print predicate color char = do
    if predicate char
                then do setSGR [SetColor Foreground Vivid color]
                        putChar char
                        setSGR [Reset]
                else do putChar char
                    
