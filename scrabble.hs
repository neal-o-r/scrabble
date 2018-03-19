import System.IO
import System.IO.Unsafe
import Data.Char
import Data.List
import Data.Foldable
--import System.Random.Shuffle
import System.Console.ANSI
import Data.List.Utils (replace)
import Text.Regex
import Text.Regex.Base
import Utils
import Data.Set (Set)
import qualified Data.Set as Set

-- definitions and IO
--board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...*...:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################"

board = "##################=..:...=...:..=##.-...;...;...-.##..-...:.:...-..##:..-...:...-..:##....-.....-....##.;...;...;...;.##..:...:.:...:..##=..:...the.:..=##..:...:.:...:..##.;...;...;...;.##....-.....-....##:..-...:...-..:##..-...:.:...-..##.-...;...;...-.##=..:...=...:..=##################"
bag = "AAAAAAAAABBCCDDDDEEEEEEEEEEEEFFGGGHHIIIIIIIIIJKLLLLMMNNNNNNOOOOOOOOPPQRRRRRRSSSSTTTTTTUUUUVVWWXYYZ__"
alphabet = ['a'..'z']
blank = '_'
bingo = 50
off = '#'
across = 1
data Play = Play {start_sq :: Int, direction :: Char
      , word:: String, rack :: String
     } deriving (Show)

dictionary = Set.fromList (lines $ up . unsafePerformIO . readFile $ "enable1.txt")
prefixes = Set.fromList (lines $ up . unsafePerformIO . readFile $ "prefixes.txt")
-- board
get_board = do
       readFile "board.txt"
-------------------------

-- Board stuff
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

is_anchor :: [Char] -> Int -> Bool
is_anchor board s = 
        let inds = (filter (\x -> x > 0 && x < (length board) - 1) [(s + i) | i <- [1, -1, 17, -17]])
         in (board !! s) == '*' || True `elem` [isAlpha (board !! i) | i <- inds]

all_anchors :: [Char] -> [Int]
all_anchors board =
        [i | i <- [0..(length board)-1], (is_anchor board i)]

------


-- Word and rack
is_word word = Set.member (up word) dictionary
is_pref word = Set.member (up word) prefixes

rack_prefixes rack =
        let expanded_rack = concatMap (subanagrams . up) (blank_expand rack blank)
         in Set.intersection (Set.fromList $ expanded_rack) prefixes

post_prefixes post prefs =
        Set.intersection (Set.map (++post) prefs) prefixes


blank_expand :: [Char] -> Char -> [[Char]]
blank_expand w b
        | b `elem` w = [replace [b] [a] w | a <- alphabet]
        | otherwise = [w]

-- get letters in rack
letters :: String -> String
letters rack =
        if blank `elem` rack
                then (letters $ filter (not . (== blank)) rack) ++ alphabet
                else up rack

-- take these tiles from rack
remove tiles rack =
        let replace_tiles = map (\c -> if (isLower c) then blank; else c)
         in rack \\ (replace_tiles tiles)
----


-- Play
make_a_play board p =
        let st  = start_sq p
            inc = if (direction p) == 'A' then 1 else 17
            end = st + (length (word p)) * inc
            inds = range inc st end
        in foldl (insertChar_in) board (zip inds (word p))

scan_to_anchor board s dir_inc =
        let c = \x -> (board !! x) /= off && not (is_anchor board x)
            i = takeWhile (c) [s + i*dir_inc | i <- [1..]]
         in last $ (s:i)

scan_to_letter board s dir_inc =
        let c = \x -> isAlpha (board !! x)
            i = takeWhile (c) [s + i*dir_inc | i <- [1..]]
         in last $ (s:i)

{-
sq_plays board rack pre s = 
        let sq = fst s
            d  = dir $ snd s
            max_pre = abs (sq - (scan_to_anchor board sq -d)) `quot` d
            pres = Set.filter (\x -> length x <= max_pre) $ rack_prefixes pre rack
        

all_plays board rack = 
        let sqs = all_sqs board
         in [sq_plays board rack sq | sq <- sqs]
-}

all_sqs board =
    let anc = all_anchors board
        rep = replicate (length anc)
     in zip (concat $ replicate 2 anc) (rep "A" ++ rep "D")  


valid_crossword cword l = (length cword) == 1 || is_word (replace "." l cword)
other_dir dir_inc = if dir_inc == 1 then 17 else 1 
dir d = if d == "D" then 17 else 1 


--main = do
--        print $ rack_prefixes "AETTER" "B"
        --board_out <- get_board
--        putStr $ board_out

