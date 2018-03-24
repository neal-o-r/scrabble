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
import Playing
import Data.Set (Set)
import qualified Data.Set as Set


dirs = [1, -1, 17, -17]
rack_len = 7

get_move_from_user board rack message err pred = do
    putStrLn message
    x <- getLine
    if pred x 
       then do
           putStrLn err
           get_move_from_user board rack message err pred
       else pure x

get_move state = do
    let board = board_state state
    let r = if even (turn state) then rack1 else rack2
    let rack = r state
    if even (turn state)
       then do putStrLn "It's Player 1's turn"
       else do putStrLn "It's Player 2's turn"

    sq_in <- (get_move_from_user board rack 
              "What square would you like to play on? (letter, number)" "Invalid Sq"
              (\x -> (square_convert x) == 0))
    d <-  (get_move_from_user board rack 
            "In what direction (Across, Down)?" "Invalid Direction" 
              (\ x -> not $ (head $ up x) `elem` ['A', 'D']))
    w <- (get_move_from_user board rack 
            "What word would you like to play (use upper case, lower to play blank)?" "That's not in your rack"
              (\ x -> False))
    let ind = square_convert sq_in
    let b = check_move (board_state state) ind d w
    if not b 
         then do
                putStrLn "Hmmm, I think you made a mistake there, try again"
                get_move state
        else pure (Play ind (head d) w rack)

check_chars b c =
    if length b == 1
        then if head $ map isAlpha b
              then [c] == b
              else True
        else True

check_move board sq d w = 
        let dir_inc = dir (head d)
            board_inds = [(sq + dir_inc * i) | (c, i) <- zip (w) [0..]]
            board_slice = [[board !! i] | i <- board_inds]
            has_anc = any (==True) $ (map (`elem` (all_anchors board)) board_inds)
            matches_letts = all (==True) [check_chars b c | (b, c) <- zip board_slice w] 
         in has_anc && matches_letts

take_letters state word =
    if even (turn state)
       then state {rack1 = remove word (rack1 state)}
       else state {rack2 = remove word (rack2 state)}

validate_sq board s c =
    let words = collect_words_around board s
        pair = [words!!0 ++ c ++ words!!2, words!!1 ++ c ++ words!!3]
        cond = \x -> is_word x || (length x) == 1
     in all (==True) (map cond pair)   

contains_achors state play = 
    let board = (board_state state)
        sq = (start_sq play)
        dir_inc = dir (direction play)
        ancs = all_anchors board
        squares = [sq + dir_inc * i | (c, i) <- zip (word play) [0..]]
     in any (==True) (map (`elem` ancs) squares)

check_play state play =
    let board = (board_state state)
        sq = (start_sq play)
        dir_inc = dir (direction play)
        valids = [validate_sq board (sq + dir_inc*i) [c] | (c, i) <- zip (word play) [0..]]
     in if (all (==True) valids) && contains_achors state play
           then play
           else play {word = ""}

tiles_used state play =
    let board = (board_state state)
        sq = (start_sq play)
        dir_inc = dir (direction play)
        letts = filter isLower [board !! (sq + dir_inc * i) | (c, i) <- zip (word play) [0..]]
     in (word play) \\ letts

l_multipliers c
  | c == ":" = 2
  | c == ";" = 3
  | otherwise = 1   

w_multipliers c
  | c == "=" = 3
  | c == "-" = 2
  | otherwise = 1


score_play state play = 
    let board = (board_state state)
        sq = (start_sq play)
        dir_inc = dir (direction play)
        board_slice = [[board !! (sq + dir_inc * i)] | (c, i) <- zip (word play) [0..]]
        word_mul = product $ map (w_multipliers) board_slice
     in word_mul * (sum [l_multipliers([board !! (sq + dir_inc*i)])*(score_letter c) | (c, i) <- zip (word play) [0..]])

update_state state play =
    let play_v = check_play state play
        new_board = make_a_play state play_v
        s = score_play state play_v
        g_up = replenish_racks (take_letters state (tiles_used state play_v))
     in if even (turn state) 
            then g_up {score1 = (score1 g_up) + s, board_state = new_board, turn=(succ (turn g_up))}
            else g_up {score2 = (score2 g_up) + s, board_state = new_board, turn=(succ (turn g_up))}

run_game state pass = do
    clearScreen
    print_board state
    p <- get_move state
    let new_state = update_state state p
    let new_pass = if (board_state state) == (board_state new_state)
                        then succ pass
                        else 0
    if pass == 2
       then do return ()
       else do run_game new_state new_pass

main = do
    bag <- shuffleM bag_in
    let init_game_state = init_racks (Game 0 0 "" "" board bag 0)
    run_game init_game_state 0
