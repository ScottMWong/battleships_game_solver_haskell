-- Project 2 Submission by Scott Wong (1082099)
-- Module contains Location type and supporting functions, feedback function for guesses
-- and guessing functions so that you can beat your opponent at this Battleship-esque game!

-- toLocation and fromLocation allow us to convert between strings and location types
-- feedback simulates the response the hider gives to the guesser.
-- GameState is used to save the list of remaining possible targets between guesses

-- Implementation of guessing follows Hints 3, 5 and 6
-- Precalculating our initial guess lets us avoid having to make ~25,000,000
-- (~5000^2) comparisons every time the program runs. 
-- For each guess (after feedback), we remove from our list of possible targets
-- all targets incompatible with the previous guess/feedback combination.
-- Then for each remaining possible target, we calculate the guess with
-- the lowest expected remaining possible targets.

module Proj2 (Location, toLocation, fromLocation, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List

-- Location consists of a char and a int (column and row)
-- write custom Show to correctly print out information
data Location = Location Char Int
  deriving (Eq)
instance Show Location where 
  show (Location a b) = [a] ++ show b

-- toLocation checks the first character is a valid column letter and 
-- number is between 1 and 4. If so we can return Just "location".
-- If EITHER is incorrect, return Nothing instead
toLocation :: String -> Maybe Location
toLocation (char:int) 
  | (cchar /= Nothing) && (cint /= Nothing) = toLocationCollect cchar cint
  | otherwise = Nothing
  where
    cchar = checkChar (char)
    cint = checkInt (read int :: Int)
    
toLocationCollect :: Maybe Char -> Maybe Int -> Maybe Location
toLocationCollect (Just char) (Just int) = Just (Location char int)

checkChar :: Char -> Maybe Char
checkChar char
 | elem char ['A','B','C','D','E','F','G','H'] = Just char
 | otherwise = Nothing

checkInt :: Int -> Maybe Int
checkInt int
 | elem int [1,2,3,4] = Just int
 | otherwise = Nothing
 
-- fromLocation calls Show on given Location
fromLocation :: Location -> String
fromLocation loc = show loc

-- feedback will take each guessed location in turn and figure out how close it is to a
-- target, then combine the information together.
feedback :: [Location] -> [Location] -> (Int,Int,Int)
feedback target_list guess_list = feedback_collect list_feedback
  where list_feedback = (map (guess_feedback target_list) guess_list)

-- feedback_collect turns the list of distances into the correct output form
feedback_collect :: [Int] -> (Int,Int,Int) 
feedback_collect [] = (0,0,0)
feedback_collect (x:xs)
 | x == 0 = (itera+1,iterb,iterc)
 | x == 1 = (itera,iterb+1,iterc)
 | x == 2 = (itera,iterb,iterc+1)
 | otherwise = (itera,iterb,iterc)
  where (itera,iterb,iterc) = feedback_collect xs

-- In order to streamline code, we use guess_feedback to get the feedback for
-- a single guessed location
guess_feedback :: [Location] -> Location -> Int
guess_feedback target_list guess = minimum(map (distance_compare guess) target_list)

-- In order to make location_feedback work, we need a function
distance_compare :: Location -> Location -> Int
distance_compare (Location chara inta) (Location charb intb)
  = max (diff (loc_char_sub chara) (loc_char_sub charb)) (diff inta intb)

-- Using diff makes distance_compare much cleaner
diff :: Int -> Int -> Int
diff a b = abs(abs(a) - abs(b))

-- loc_chara_sub helps us compare the distance between columns of locations 
loc_char_sub :: Char -> Int
loc_char_sub c
 | c == 'A' = 1
 | c == 'B' = 2
 | c == 'C' = 3
 | c == 'D' = 4
 | c == 'E' = 5
 | c == 'F' = 6
 | c == 'G' = 7
 | c == 'H' = 8
 | otherwise = -1
 
-- A GameState passes on  the list of remaining target combos
type GameState = [[Location]]

-- get_board_list returns full list of locations possible on the board
get_board_list :: [Location]
get_board_list = concat (map (create_loc_int char_list) int_list)
  where
    char_list = ['A','B','C','D','E','F','G','H']
    int_list = [1,2,3,4]

-- create_loc_int maps all Chars to an Int for create of Locations
create_loc_int :: [Char] -> Int -> [Location]
create_loc_int char_list int = map (create_loc int) char_list

-- Need to put Char first to make a location
create_loc :: Int -> Char -> Location
create_loc int char = Location char int

-- Get all combos of three locations from the board
choose_three :: [Location] -> [[Location]]
choose_three board = choose board 3

-- Standard choose function, not used except in choose_three
choose :: [Location] -> Int -> [[Location]]
choose _ 0 = [[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys) (choose xs (n-1))) ++ (choose xs n)

-- initialGuess has been pre-computed using the same functions as nextGuess,
-- except with all locations as input. 
-- There were multiple initial guesses with the same expected value,
-- and we could've chosen any of them.
-- We also need to generate a list of all the possible guesses to pare down later.
initialGuess :: ([Location],GameState)
initialGuess = ([Location 'A' 1, Location 'H' 1, Location 'A' 3], all_guesses)
  where
    all_guesses = choose_three get_board_list

-- Remove all incompatible guesses, then get best remaining guess using 
-- remaining targets
nextGuess :: ([Location],GameState) -> (Int,Int,Int) -> ([Location],GameState)
nextGuess (last_guess,last_state) last_feedback = (guess, current_state)
  where
    current_state = apply_feedback last_guess last_state last_feedback
    guess = get_best_guess current_state
    
-- apply_feedback removes all targets which are not compatible with the feedback
-- given by last guess
apply_feedback :: [Location] -> GameState -> (Int,Int,Int) -> GameState
apply_feedback last_guess last_state exp_fb = filter (\x -> fb_check x last_guess exp_fb) last_state

-- fb_check returns True if feedback check_target last_guess == exp_fb
fb_check :: [Location] -> [Location] -> (Int,Int,Int) -> Bool
fb_check check_target last_guess exp_fb = (feedback check_target last_guess) == exp_fb

-- Assuming the input list contains all possible targets, get_best_guess
-- will return the guess with the lowest expected possible targets remaining
-- Following Hint 6, we can obtain a value for each list from expected
get_best_guess :: [[Location]] -> [Location]
get_best_guess guess_list = output
  where 
    sorted_value_and_guess = sortBy guess_sort (map (calc_guess_exp_value guess_list) guess_list)
    (_,output) = head sorted_value_and_guess

-- relative value can be obtained from the square of the length of each feedback
-- list. basically, the more equally likely each feedback is, the more reliably
-- we can cut down on the list of targets.
-- for example, if there are 3 distinct possible feedbacks, a distribution of
-- 1/2/12 gives a 20% chance to win big, but 1/7/7 gives a guaranteed halving in
-- possible cases.
-- rem_comb is remaining combinations, guess is three guessed locations
-- guess value takes the grouped sorted list of feedbacks for guess_list
calc_guess_exp_value :: [[Location]] -> [Location] -> (Int , [Location])
calc_guess_exp_value guess_list guess = (value, guess)
  where
    value = guess_value (group (sort (map (guess_first_feedback guess) guess_list)))

-- we use this to map in cal_guess_exp_value
-- because feedback wants target first but we want to map across target list
guess_first_feedback :: [Location] -> [Location] -> (Int, Int, Int)
guess_first_feedback guess target = feedback target guess

-- we don't actually need to do any division here because
-- the denominator would be the same for all values being compared
guess_value :: [[a]] -> Int
guess_value [] = 0
guess_value (x:xs) = ((length x) ^ 2) + guess_value xs

-- We only care about sorting by value
guess_sort (a, _) (b, _)
  | a < b = LT
  | a == b = EQ
  | a > b = GT
