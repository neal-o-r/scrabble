module Playing(is_in_rack
, make_a_play
, scan_to_anchor
, scan_to_letter
, square_convert
, collect_words_around
, collect_letters
, all_sqs
, rev_dir)
    where

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
import Board
import Data.Set (Set)
import qualified Data.Set as Set

dirs = [1, -1, 17, -17]
rack_len = 7

is_in_rack w rack
    | w == "" = True
    | otherwise = (head w) `elem` (letters rack) && (is_in_rack (tail w) (remove [(head w)] rack))

-- Play
make_a_play state p =
        let st  = start_sq p
            inc = dir (direction p)
            end = st + (length (word p)) * inc
            inds = range inc st end
         in foldl (insertChar_in) (board_state state) (zip inds (low $ word p))

scan_to_anchor board s dir_inc =
        let c = \x -> (board !! x) /= off && not (is_anchor board x)
            i = takeWhile (c) [s + i*dir_inc | i <- [1..]]
         in last $ (s:i)

scan_to_letter board s dir_inc =
        let c = \x -> isAlpha (board !! x)
            i = takeWhile (c) [s + i*dir_inc | i <- [1..]]
         in last $ (s:i)

square_convert sq_in =
    let l = findIndex (== head (up sq_in)) ['A'..'O']
        n = read (tail $ tail sq_in) :: Int
     in if n > 0 && n < 16 && not (isNothing l)
           then (fromJust l * 17) + n + 17
           else 0

rev_dir s i
    | i > 0 = s
    | otherwise = reverse s

collect_words_around board s =
    let s_a = zip (map (s+) dirs) dirs
        clb = collect_letters board
     in map (\ x -> rev_dir (clb (fst x) (snd x)) (snd x)) s_a

collect_letters board s dir_inc =
    let w = [board !! i | i <- (range dir_inc s (scan_to_letter board s dir_inc))]
     in if not (isAlpha $ w !! 0) then "" else w

all_sqs board =
    let anc = all_anchors board
        rep = replicate (length anc)
     in zip (concat $ replicate 2 anc) (rep "A" ++ rep "D")

