module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
                  deriving (Eq, Show)

-- Exercise one

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
a +++ b = Append (tag a `mappend` tag b) a b

-- Exercise two

instance (Sized b, Monoid b) => Sized (JoinList b a) where
  size = size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a)   = Just a
indexJ n (Append _ a b) 
 | n < m     = indexJ n a
 | otherwise = indexJ (n - m) b
   where m = getSize $ size a
indexJ _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty

