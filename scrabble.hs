import System.IO
import Data.Char
import Data.List
import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base
import My_utils

-- definitions
board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...*...:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################"

bag = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"
alphabet = ['a'..'z']
blank = '_'
bingo = 50
across = 1
data Play = Play {start_sq :: Int, direction :: Char
      , word:: String, rack :: String
     } deriving (Show)

-- read dictionary
get_words = do
        readFile "enable1.txt"

-- board
get_board = do
       readFile "board.txt"


-- convert the internal ascii board to the prinable board
-- an annoyingly large number of magic numbers in here
board_convert board_ascii board_out =
        let re = makeRegex "[a-z|A-Z]" :: Regex
            letter_inds = regex_indices re (replace "#" "" board_ascii)
            letts = filter isAlpha board_ascii
            row_offset = 80
            row_no = map (`quot` 15) letter_inds
            col_no = map (`mod` 15) letter_inds
            cols = add_num 6 (mul_num 5 col_no)
            rows = (add_num (2*row_offset) (mul_num (2*row_offset+2) row_no))
            true_inds = add_arr rows cols
        in foldl (insertChar) board_out (zip true_inds letts)


print_board b =
        let printer = color_print isAlphaNum White
         in sequence_ (map printer b)


is_word word dictionary = word `elem` dictionary

-- get letters in rack
letters :: String -> String
letters rack =
        if blank `elem` rack
                then (letters $ filter (not . (== blank)) rack) ++ alphabet
                else nub (map toUpper rack)

-- take these tiles from rack
remove tiles rack =
        let replace_tiles = map (\c -> if (isLower c) then blank; else c)
         in rack \\ (replace_tiles tiles)

make_a_play board p =
        let st  = start_sq p
            inc = if (direction p) == 'A' then 1 else 17
            end = st + (length (word p)) * inc
            inds = range inc st end
        in foldl (insertChar_in) board (zip inds (word p))


main = do
        dictionary <- get_words
--        board_out <- get_board
--        putStr $ board_out

