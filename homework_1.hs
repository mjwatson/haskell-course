------------------------------------------------------
-- Luhn checksum exercise
------------------------------------------------------

-- Convert an Integer to a list of its digits

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n <= 0    = []
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- Double every other integer beginning from the right
-- eg [1,2,3,4] -> [2,2,6,4]

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev (x : y : xs) = x : (2 * y) : (doubleEveryOtherRev xs)
doubleEveryOtherRev xs = xs

doubleEveryOther = reverse . doubleEveryOtherRev . reverse

-- Sum digits of all numbers in the list

sumDigit :: Integer -> Integer
sumDigit = sum . toDigits

sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x : xs) = sumDigit x + sumDigits xs 

-- Validate the luhn checksum
-- validate 4012888888881881 = True
-- validate 4012888888881882 = False

validate :: Integer -> Bool
validate n = (checksum n `mod` 10) == 0
  where checksum = sumDigits . doubleEveryOtherRev . toDigitsRev

------------------------------------------------------
-- Towers of hanoi exercise
------------------------------------------------------

type Peg  = String
type Move = (Peg, Peg)

-- Given the number of disks, and names for each peg, return the moves to complete tower of hanoi
-- Example: hanoi 2 "a" "b" "c" == [("a","c"), ("a","b"), ("c","b")]
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0    = []
  | otherwise = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)




