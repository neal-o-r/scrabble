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
, board_out
, score_letter
, Game(..)
, Play(..)
) where

import Data.Char
import Utils
import System.IO
import System.IO.Unsafe
import Data.Set (Set)
import qualified Data.Set as Set


data Game = Game {score1 :: Integer, score2 :: Integer
                 , rack1 :: String, rack2 :: String
                 , board_state :: String, bag :: String, turn :: Int} deriving (Show)
data Play = Play {start_sq :: Int, direction :: Char
                 , word:: String, rack :: String
                 } deriving (Show, Read)

-- definitions and IO
board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...*...:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################"

bag_in = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"
alphabet = ['a'..'z']
blank = '_'
bingo = 50
off = '#'
across = 1

other_dir dir_inc = if dir_inc == 1 then 17 else 1
dir d = if d == 'D' then 17 else 1

dictionary = Set.filter (\x -> length x < 10) $ Set.fromList (lines $ up . unsafePerformIO . readFile $ "enable1.txt")
board_out = unsafePerformIO . readFile $ "board.txt"
score_letter = score . toLower
 where
  score 'a' = 1
  score 'e' = 1
  score 'i' = 1
  score 'o' = 1
  score 'u' = 1
  score 'l' = 1
  score 'n' = 1
  score 'r' = 1
  score 's' = 1
  score 't' = 1
  score 'd' = 2
  score 'g' = 2
  score 'b' = 3
  score 'c' = 3
  score 'm' = 3
  score 'p' = 3
  score 'f' = 4
  score 'h' = 4
  score 'v' = 4
  score 'w' = 4
  score 'y' = 4
  score 'k' = 5
  score 'j' = 8
  score 'x' = 8
  score 'q' = 10
  score 'z' = 10
