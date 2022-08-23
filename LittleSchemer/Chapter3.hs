module LittleSchemer.Chapter3 where

-- rember
rember :: [Char] -> [[Char]] -> [[Char]]
rember a [] = []
rember a (x:xs) = if x == a then xs else x:rest where
  rest = rember a xs

-- firsts: takes the first element of a list
firsts :: [[[Char]]] -> [[Char]]
firsts [] = []
firsts (x:xs) = head x:rest where
  rest = firsts xs


-- insertR: builds a list with NEW inserted to the right of the first occurence of OLD

type New = [Char]
type Old = [Char]
type Lst = [[Char]]

insertR :: Lst -> Old -> New -> [[Char]]
insertR [] old new = []
insertR (x:xs) old new = if x == old then old:rest else x:rest2 where
  rest = new:xs
  rest2 = insertR xs old new


-- InsertL: builds a list with NEW inserted to the left of the first occurence of OLD
insertL :: Lst -> Old -> New -> [[Char]]
insertL [] old new = []
insertL (x:xs) old new = if x == old then new:rest else x:rest2 where
  rest = old:xs
  rest2 = insertL xs old new

-- subst: replaces either the first occurence of O1 or the first occurence of O2 by NEW

subst :: [[Char]] -> [Char] -> [Char] -> [Char] -> [[Char]]
subst [] new o1 o2 = []
subst (x:xs) new o1 o2 = if x == o1 || x == o2 then new:xs else x:rest where
  rest = subst xs new o1 o2

-- multirember: returns a list LST with all occurences of A removed

multirember :: [[Char]] -> [Char] -> [[Char]]
multirember [] a = []
multirember (x:xs) a = if x == a then multirember xs a else x:rest where
  rest = multirember xs a
