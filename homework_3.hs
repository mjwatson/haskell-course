module Golf where 

import Data.List

-- Exercise 1: Skips
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should contain 
-- every second element from the input list and the nth list in the output 
-- should contain every nth element from the input list.
--
-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []

nth :: Int -> [a] -> [a]
nth n l = case drop (n-1) l of 
            (x:xs) -> x : nth n xs
            []     -> []

skips :: [a] -> [[a]]
skips l = map (`nth` l) [1..(length l)]

-- Exercise 2: Local maxima
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, 
-- since it is greater than the elements immediately before and after it 
-- (3 and 1). 5 is not a local maximum since there is no element that comes
-- after it
--
-- localMaxima [2,9,5,6,1] == [9,6]
-- localMaxima [2,3,4,1,5] == [4]
-- localMaxima [1,2,3,4,5] == []

isLocalMaxima :: (Integer, Integer, Integer) -> Bool
isLocalMaxima (x,y,z) = x < y && z < y

readLocalMaxima :: (Integer, Integer, Integer) -> Integer
readLocalMaxima (_,y,_) = y

localMaxima :: [Integer] -> [Integer]
localMaxima l =  map readLocalMaxima $ filter isLocalMaxima $ zip3 l (drop 1 l) (drop 2 l)

-- Exercise 3: Histogram
-- For this task, write a function histogram :: [Integer] -> String
-- which takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers).

newline = "\n"       
line    = "=========="
index   = "0123456789"

star :: [Integer] -> Integer -> Integer -> Char
star ns n m = if (n <= (toInteger $ length (filter (== m) ns))) then '*' else ' '

row :: [Integer] -> Integer -> String
row ns n = map (star ns n) [0..9]

rows :: [Integer] -> [String]
rows ns = reverse $ map (row ns) [1..9]

histogram :: [Integer] -> String
histogram ns = intercalate newline (rows ns ++ [line, index])
