{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements sorting functions that meet some properties of a correct sorting function.
-}

module Ex02 where

import Data.List
import Test.QuickCheck

-- the following functions meet some properties of a correct sorting function
dodgySort1,dodgySort2,dodgySort3,dodgySort4 :: [Int] -> [Int]

-- meets prop 2 & 4, but not prop 1 & 3 & 5
dodgySort1 xs = xs

-- meets prop 1 & 2 & 3, but not prop 4 & 5
dodgySort2 xs = insertionSort $ 1 : xs

-- meets prop 1 & 3 & 4, but not prop 2 & 5
dodgySort3 xs = insertionSort $ swapMaxElem xs
  where
    max = maximum xs

    -- removes 1st occurrence of max and prepends max + 1 to the list
    swapMaxElem :: [Int] -> [Int]
    swapMaxElem [] = []
    swapMaxElem xs = (max + 1) : (delete max xs)

-- meets prop 1 & 2 & 3 & 4, but not prop 5
dodgySort4 xs = insertionSort newList
  where
    newList = case findDuplicate (insertionSort xs) of
      Just dup -> (1 : (delete dup xs)) -- could find duplicate
      _        -> xs -- no duplicate found

    -- finds a duplicate in a list or returns Nothing
    findDuplicate :: [Int] -> Maybe Int
    findDuplicate [] = Nothing
    findDuplicate (x:xs) = if frequency x (x:xs) > 1
                           then Just x -- found duplicate
                           else findDuplicate xs -- check rest of list

    -- finds the frequency of an element of a list
    frequency :: Int -> [Int] -> Int
    frequency x [] = 0
    frequency x (y:ys) = if x == y
                         then 1 + frequency x ys
                         else frequency x ys

-- sorting function should produce the same list for a reversed list
sortProp1 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp1 sortFn xs = sortFn xs == sortFn (reverse xs)

-- sorting function should not lose elements
sortProp2 :: ([Int] -> [Int]) -> Int -> [Int] -> [Int] -> Bool
sortProp2 sortFn x xs ys = x `elem` sortFn (xs ++ [x] ++ ys)

-- sorting function should put elements in ascending order
sortProp3 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp3 sortFn xs = isSorted (sortFn xs)
  where
    isSorted (x1 : x2 : xs) = (x1 <= x2) && isSorted (x2 : xs)
    isSorted _ = True

-- sorting function should not change the length of the list
sortProp4 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp4 sortFn xs = length xs == length (sortFn xs)

-- checks sorting function against correct version
sortProp5 :: ([Int] -> [Int]) -> [Int] -> Bool
sortProp5 sortFn xs
  = sortFn xs == insertionSort xs

-- correct (but inefficient) sorting function
insertionSort :: [Int] -> [Int]
insertionSort xs = foldr insertSorted [] xs
  where
    insertSorted x [] = [x]
    insertSorted x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insertSorted x ys

-- less verbose version of quickCheck
quickCheck' prop
  = output <$> quickCheckWithResult stdArgs{chatty = False, maxSuccess = 100} prop
