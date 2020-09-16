module DList where

import Test.HUnit

type DList a = [a] -> [a]

list :: [Int]
list = 1 : 2 : 3 : [] -- end is nil

dlist :: DList Int
dlist = \x -> 1 : 2 : 3 : x -- end is "x"

empty :: DList a
empty = id

-- not good but works: empty = \x -> x

singleton :: a -> DList a
singleton x = (x :)

-- not good but works: singleton x = \y -> x : y
-- singleton x = (:)

append :: DList a -> DList a -> DList a
-- not good but works: append dl1 dl2 = \x -> dl1 (dl2 x)
append = (.)

cons :: a -> DList a -> DList a
-- not good but works: cons x dl = append (singleton x) dl
cons = append . singleton

fromList :: [a] -> DList a
fromList [] = empty
-- not good but works: fromList (h : t) = cons h (fromList t)
fromList xs = (xs ++)

toList :: DList a -> [a]
toList x = x []

testDList :: IO ()
testDList = do
  _ <-
    runTestTT $
      TestList
        [ toList empty ~?= ([] :: [Char]),
          toList (singleton 1) ~?= ([1] :: [Int]),
          toList (append (singleton 1) empty) ~?= ([1] :: [Int]),
          toList (append (singleton 1) (singleton 2)) ~?= ([1, 2] :: [Int])
        ]
  return ()

micro1 :: Char
micro1 = last (t 10000 "")
  where
    t 0 l = l
    t n l = t (n -1) (l ++ "s")

micro2 :: Char
micro2 = last (toList (t 10000 empty))
  where
    t 0 l = l
    t n l = t (n -1) (l `append` singleton 's')

naiveReverse :: [a] -> [a]
naiveReverse = rev
  where
    rev [] = []
    rev (x : xs) = rev xs ++ [x]

bigList :: [Int]
bigList = [0 .. 10000]

micro3 :: Int
micro3 = last (naiveReverse bigList)

ivoryTowerReverse :: [a] -> [a]
ivoryTowerReverse = foldr (flip (++) . (: [])) []

micro4 :: Int
micro4 = last (ivoryTowerReverse bigList)

dlistReverse :: [a] -> [a]
dlistReverse = toList . rev
  where
    rev [] = empty
    rev (x : xs) = rev xs `append` singleton x

micro5 :: Int
micro5 = last (dlistReverse bigList)

dlistIvoryTowerReverse :: [a] -> [a]
dlistIvoryTowerReverse = toList . foldr (flip append . singleton) empty

micro6 :: Int
micro6 = last (dlistIvoryTowerReverse bigList)

micro7 :: Int
micro7 = last (reverse bigList)
