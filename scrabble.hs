import System.IO  
import Data.Char
import Data.List
import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base

add_arr a b = zipWith (+) a b 
mul_arr a b = zipWith (*) a b 
mul_num a b = map (* a) b
add_num a b = map (+ a) b

-- definitions
board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...*...:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################" 

bag = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"
alphabet = ['a'..'z']
blank = '_'
bingo = 50
across = 1
data Play = Play {start_sq :: Int, direction :: Char 
                  , letter_state:: String, rack :: String 
                 } deriving (Show)

-- read dictionary
get_words = do
        readFile "enable1.txt"

-- board
get_board = do
        readFile "board.txt"


replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

-- put in a char at a place, and a blank space after
insertChar :: String -> (Int, Char) -> String
insertChar board state = 
        let loc = fst state
            c = snd state 
         in replaceAtIndex (succ loc) ' ' (replaceAtIndex loc c board)


regex_indices :: Regex -> String -> [Int]
regex_indices reg str = [ i | (i,x) <- zip [0..] str, (matchTest reg [x])] 

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


-- print something with color, 
-- give a predicate on which color changes
-- color to change to
color_print predicate color char = do
    if predicate char
                then do setSGR [SetColor Foreground Vivid color] 
                        putChar char
                        setSGR [Reset]
                else do putChar char

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



main = do
        --dictionary <- get_words
        board_out <- get_board
        print_board (board_convert board board_out)
