import Control.Applicative

-- Questions from lecture
-- Reimplementing standard control features

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g = sequenceA . map g

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n = sequenceA . replicate n


-- Homework
