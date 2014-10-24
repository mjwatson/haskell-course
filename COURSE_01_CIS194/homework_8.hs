module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL es fun) = GL (employee : es) (fun + empFun employee)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL es fun) (GL es' fun') = GL (es ++ es') (fun + fun')

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ a') b@(GL _ b')
  | a' < b'   = b
  | otherwise = a

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a bs) = f a (map (treeFold f) bs)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss gls = (bestWithBoss, bestWithoutBoss)
  where bestWithBoss    = glCons boss $ mconcat $ map snd gls
        bestWithoutBoss = mconcat $ map (uncurry moreFun) gls

maxFun :: Tree Employee -> GuestList
maxFun = (uncurry moreFun) . (treeFold nextLevel)

processData :: String -> String
processData contents = output
  where employees        = read contents
        guesslist        = maxFun employees
        (GL invited fun) = guesslist
        totalline        = "Total fun: " ++ (show fun)
        invitedlist      = sort $ map empName invited
        output           = unlines $ totalline : invitedlist


processFile :: String -> IO ()
processFile path = readFile path >>= (putStr . processData)

main = processFile "company.txt"


