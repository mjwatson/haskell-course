{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

dice :: Int -> Rand StdGen DieValue -> Rand StdGen [DieValue]
dice n die = (replicateM n die) 

sortedDice :: Int -> Rand StdGen DieValue -> Rand StdGen [DieValue]
sortedDice n die = dice n die >>= \ds -> return (reverse $ sort ds) 

attackDice :: Battlefield -> Int
attackDice = min 3 . subtract 1 . attackers

defenceDice :: Battlefield -> Int
defenceDice = min 2 . defenders

loss :: Battlefield -> DieValue-> DieValue-> Battlefield
loss (Battlefield n m) (DV a) (DV b) 
  | a <= b    = Battlefield (n-1) m
  | otherwise = Battlefield n (m-1)

losses :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
losses bf [] _ = bf
losses bf _ [] = bf
losses bf (a:as) (b:bs) = losses (loss bf a b) as bs

battle :: Battlefield -> Rand StdGen Battlefield
battle bf = sortedDice (attackDice bf) die >>= \ad ->
            sortedDice (defenceDice bf) die >>= \dd ->
            return (losses bf ad dd)

invade :: Battlefield -> Rand StdGen Battlefield
invade bf | complete bf  = return bf
          | otherwise    = battle bf >>= invade
    where complete bf = attackers bf < 2  || 
                        defenders bf == 0

calculateResult :: [Battlefield] -> Double
calculateResult bfs = (fromIntegral (length $ filter ((== 0) . defenders) $ bfs)) / 1000

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= (\bfs -> return $ calculateResult bfs)


        


