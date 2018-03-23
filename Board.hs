module Board (player_rack
, board_replacements
, board_convert
, print_board
, all_anchors
, is_anchor
, letters_played
, is_word
, from_bag
, blank_expand
, letters
, remove
, replenish_racks
, fill_rack
, init_racks 
) where

import System.IO
import System.IO.Unsafe
import Data.Char
import Data.List
import Data.Foldable
import Data.Maybe
import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base
import Utils
import Defs
import Data.Set (Set)
import qualified Data.Set as Set

dirs = [1, -1, 17, -17]
rack_len = 7

-- Board stuff
-- convert the internal ascii board to the prinable board
-- an annoyingly large number of magic numbers in here
player_rack state =
    if even (turn state)
       then (rack1 state)
       else (rack2 state)

board_replacements state =
    let b1 = replace "'Rack_'" (player_rack state) board_out
        b2 = replace "'Player 1 Score'" (show $ score1 state) b1
     in replace "'Player 2 Score'" (show $ score2 state) b2


board_convert state =
        let board_ascii = (board_state state)
            re = makeRegex "[a-z|A-Z]" :: Regex
            letter_inds = regex_indices re (replace "#" "" board_ascii)
            letts = filter isAlpha board_ascii
            row_offset = 80
            row_no = map (`quot` 15) letter_inds
            col_no = map (`mod` 15) letter_inds
            cols = add_num 6 (mul_num 5 col_no)
            rows = (add_num (2*row_offset) (mul_num (2*row_offset+2) row_no))
            true_inds = add_arr rows cols
            board_print = board_replacements state
         in foldl (insertChar) board_print (zip true_inds letts)

print_board state =
    sequence_ (map color_print (board_convert state))

is_anchor :: [Char] -> Int -> Bool
is_anchor board s =
        let inds = (filter (\x -> x > 0 && x < (length board) - 1) [(s + i) | i <- dirs])
         in (board !! s) == '*' || True `elem` [isAlpha (board !! i) | i <- inds]

all_anchors :: [Char] -> [Int]
all_anchors board =
        [i | i <- [0..(length board)-1], (is_anchor board i)]

letters_played board =
    length $ filter isLower board

is_word word = Set.member (up word) dictionary

from_bag state n =
    let used = (letters_played (board_state state)) + (length $ rack1 state) + (length $ rack2 state)
     in slice used (used+n) (bag state)
init_racks s =
    let g1 = (Game 0 0 (from_bag s rack_len) "" (board_state s) (bag s) 0)
     in (Game 0 0 (rack1 g1) (from_bag g1 rack_len) (board_state s) (bag s) 0)

fill_rack state rack = rack ++ (from_bag state (rack_len - (length rack)))

replenish_racks state =
    let g1 = state {rack1 = fill_rack state (rack1 state)}
     in g1 {rack2 = fill_rack g1 (rack2 g1)}

blank_expand :: [Char] -> Char -> [[Char]]
blank_expand w b
        | b `elem` w = [replace [b] [a] w | a <- alphabet]
        | otherwise = [w]

-- get letters in rack
letters :: String -> String
letters rack =
        if blank `elem` rack
                then (letters $ filter (not . (== blank)) rack) ++ alphabet
                else nub $ up rack

-- take these tiles from rack
remove tiles rack =
        let replace_tiles = map (\c -> if (isLower c) then blank; else c)
         in rack \\ (replace_tiles tiles)
