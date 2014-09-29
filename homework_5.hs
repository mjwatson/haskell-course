{-# LANGUAGE TypeSynonymInstances #-}

import Parser
import StackVM

-- ExprT
-- Note constructors succeeded with T (AddT etc) to avoid conflicts with version imported from StackVM.hs
data ExprT = LitT Integer 
           | AddT ExprT ExprT 
           | MulT ExprT ExprT
             deriving (Show, Eq)

-- Exercise 1: Version one of the calculator

eval :: ExprT -> Integer
eval (LitT n)   = n
eval (AddT a b) = eval a + eval b
eval (MulT a b) = eval a * eval b

-- Exercise 2: Support parse
-- Note working without internet connectivity so
-- hardcoding parse function based on examples in exercise

evalStr :: String -> Maybe Integer
evalStr s = case parseExp LitT AddT MulT s of 
              Just e  -> Just $ eval e
              Nothing -> Nothing

class Expr a where
  lit :: (Integer -> a)
  add :: (a -> a -> a)
  mul :: (a -> a -> a)

instance Expr ExprT where
  lit = LitT
  add = AddT
  mul = MulT

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (0 <=)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Ord MinMax where
  (MinMax a) < (MinMax b)  = a < b
  (MinMax a) <= (MinMax b) = a <= b

instance Expr MinMax where
  lit = MinMax
  add = max
  mul = min
  
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit = Mod7 . (`mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 $ (a + b) `mod` 7 
  mul (Mod7 a) (Mod7 b) = Mod7 $ (a * b) `mod` 7 

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3*-4)+5" 

-- Exercise 5

instance Expr Program where
  lit n   = [ PushI n ] 
  add a b = a ++ b ++ [ Add ]
  mul a b = a ++ b ++ [ Mul ]



