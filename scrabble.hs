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


data Game = Game {score1 :: Int, score2 :: Int
                 , rack1 :: String, rack2 :: String
                 , board_state :: String, bag :: String} deriving (Show)
data Play = Play {start_sq :: Int, direction :: Char
                 , word:: String, rack :: String
                 } deriving (Show, Read)
dirs = [1, -1, 17, -17]
rack_len = 7

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
         sequence_ (map color_print b)

is_anchor :: [Char] -> Int -> Bool
is_anchor board s = 
        let inds = (filter (\x -> x > 0 && x < (length board) - 1) [(s + i) | i <- dirs])
         in (board !! s) == '*' || True `elem` [isAlpha (board !! i) | i <- inds]

all_anchors :: [Char] -> [Int]
all_anchors board =
        [i | i <- [0..(length board)-1], (is_anchor board i)]

letters_played board =
    length $ filter isLower board
------


-- Word and rack
is_word word = Set.member (up word) dictionary

from_bag state n = 
    let used = (letters_played (board_state state)) + (length $ rack1 state) + (length $ rack2 state)
     in slice used (used+n) (bag state)
init_racks s = 
    let g1 = (Game 0 0 (from_bag s rack_len) "" (board_state s) (bag s))
     in (Game 0 0 (rack1 g1) (from_bag g1 rack_len) (board_state s) (bag s))
  

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
----


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
    let l = findIndex (== head sq_in) ['A'..'O']
        n = read (tail $ tail sq_in) :: Int
     in if n > 0 && n < 16 && not (isNothing l)
           then (fromJust l * 17) + n + 17
           else 0

is_in_rack w rack
    | w == "" = True
    | otherwise = (head w) `elem` (letters rack) && (is_in_rack (tail w) (remove [(head w)] rack))


get_move_from_user board rack message err pred = do
    putStrLn message
    x <- getLine
    if pred x 
       then do
           putStrLn err
--           get_move board rack
       else return $ read x
{-
get_move board rack = 
    let sq_in = (get_move_from_user board rack
            "What square would you like to play on? (letter, number)" "Invalid Sq"
            (\x -> ((square_convert x) == 0) || (not $ (square_convert x) `elem` (all_anchors board) )))
    in square_convert sq_in
-}

{-
get_move board rack =
    let sq_in = (get_data_from_user board rack 
                  "What square would you like to play on (letter, number)?" "Invalid Sq"  
                 (\x -> (x == 0) || not ((square_convert x) `elem` (all_anchors board))))
        d = (get_data_from_user board rack 
            "In what direction (Across, Down)?" "Invalid Direction" 
              (\ x -> not $ (head x) `elem` ['A', 'D']))
        w = (get_data_from_user board rack 
            "What word would you like to play (use upper case, lower to play blank)?" "That's not in your rack"
              (\ x -> not $ is_in_rack x rack))
        ind = square_convert sq_in
    in (Play ind (head d) w rack)
-}

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

main = do
    board_out <- get_board
    bag <- shuffleM bag_in
    let init_game_state = init_racks (Game 0 0 "" "" board bag)
    print_board $ board_convert board board_out


