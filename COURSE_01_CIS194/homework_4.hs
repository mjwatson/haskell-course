import Data.List

-- Exercise 1: Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)  
 | even x = (x - 2) * fun1 xs
 | otherwise = fun1 xs  

fun1' :: [Integer] -> Integer
fun1' = product . map (+ (-2)) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n    = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)


fun2'' :: Integer -> Integer
fun2'' n
  | n == 1    = 0
  | even n    = n `div` 2
  | otherwise = 3 * n + 1

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 0) . iterate fun2''

-- Exercise 2: Folding with trees

data Tree a = Leaf | Node Integer (Tree a) a (Tree a) deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1 
height (Node n _ _ _) = n

node :: Tree a -> a -> Tree a -> Tree a
node l v r = Node (1 + max (height l) (height r)) l v r

insertNode :: a -> Tree a -> Tree a
insertNode value Leaf = node Leaf value Leaf
insertNode value (Node _ l v r)  
  | height l < height r = node (insertNode value l) v r
  | otherwise           = node l v (insertNode value r) 
  
foldTree :: [a] -> Tree a
foldTree = foldr insertNode Leaf 

-- Exercise 3: More folds

xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a->b) -> [a] -> [b]
map' _ []     = []
map' f (x:xs) = f x : map' f xs

-- Exercise 4: Finding primes

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

form :: (Integer, Integer) -> Integer
form (a,b) = a + b + (2 * a * b)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) ns
  where ns = foldr (delete . form) [1..n] (cartProd [1..n] [1..n])



