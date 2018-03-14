module My_utils
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
, low)
where

import System.IO
import Data.Char
import Data.List
import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base

range step start end = takeWhile (<=end) $ iterate (+step) start
add_arr a b = zipWith (+) a b
mul_arr a b = zipWith (*) a b
mul_num a b = map (* a) b
add_num a b = map (+ a) b

up s = map toUpper s
low s = map toLower s

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
                    
