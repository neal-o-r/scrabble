module Defs
( board
, bag_in
, alphabet
, blank
, bingo
, off
, across
, other_dir
, dir
, dictionary
, prefixes
, get_board)
where

import Utils
import System.IO
import System.IO.Unsafe
import Data.Set (Set)
import qualified Data.Set as Set


-- definitions and IO
board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...*...:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################"

bag_in = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"
alphabet = ['a'..'z']
blank = '_'
bingo = 50
off = '#'
across = 1
data Play = Play {start_sq :: Int, direction :: Char
                 , word:: String, rack :: String
                 } deriving (Show)


other_dir dir_inc = if dir_inc == 1 then 17 else 1
dir d = if d == 'D' then 17 else 1

dictionary = Set.filter (\x -> length x < 10) $ Set.fromList (lines $ up . unsafePerformIO . readFile $ "enable1.txt")
prefixes = Set.fromList (lines $ up . unsafePerformIO . readFile $ "prefixes.txt")
-- board
get_board = do
        readFile "board.txt"

