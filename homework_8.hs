module Party where

import Employee

glCons :: Employee -> GuestList -> GuestList
glCons employee (GL es fun) = GL (employee : es) (fun + empFun employee)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ a') b@(GL _ b')
  | a' < b' = b
  | otherwise    = a

