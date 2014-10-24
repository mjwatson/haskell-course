-- Fibinacci

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = map fst $ iterate fibstep (0,1)
         where fibstep (a,b) = (b, a+b)

-- Streams

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : streamToList s

instance Show a => Show (Stream a) where 
  show  = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a s) = Cons (f a) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons a (streamFromSeed f (f a))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a s) t = Cons a (interleaveStreams t s)

ruler :: Stream Integer
ruler = interleaveStreams (streamRepeat 0) (streamMap (+ 1) ruler)

-- Generating functions

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))



