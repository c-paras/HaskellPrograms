{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements a Binary Tree and common operations.
-}

module Ex03 (
  BinaryTree(..), isBST, insert, deleteAll, searchTrees, isSorted,
  sortedListsWithoutDuplicates, isBalanced,
  mysteryPred, mysterious, astonishing
) where

import Test.QuickCheck
import Data.List(sort, nub, delete)

-- defines a BinaryTree
data BinaryTree
  = Branch Integer BinaryTree BinaryTree
  | Leaf
  deriving (Show, Ord, Eq)

-- predicate that checks whether a BinaryTree is a BST
isBST :: BinaryTree -> Bool
isBST Leaf = True
isBST (Branch v l r)
  = allTree (< v)  l &&
    allTree (>= v) r &&
    isBST l          &&
    isBST r
  where allTree :: (Integer -> Bool) -> BinaryTree -> Bool
        allTree f (Branch v l r) = f v && allTree f l && allTree f r
        allTree f (Leaf) = True

-- adds an Integer to a BinaryTree, preserving BST property
insert :: Integer -> BinaryTree -> BinaryTree
insert i Leaf = Branch i Leaf Leaf
insert i (Branch v l r)
  | i < v     = Branch v (insert i l) r
  | otherwise = Branch v l (insert i r)

-- removes all instances of an Integer in a BinaryTree, preserving BST property
deleteAll :: Integer -> BinaryTree -> BinaryTree
deleteAll i Leaf = Leaf
deleteAll i (Branch j Leaf r) | i == j = deleteAll i r
deleteAll i (Branch j l Leaf) | i == j = deleteAll i l
deleteAll i (Branch j l r) | i == j = let (x, l') = deleteRightmost l
                                       in Branch x l' (deleteAll i r)
                           | i <  j = Branch j (deleteAll i l) r
                           | i >  j = Branch j l (deleteAll i r)
  where deleteRightmost :: BinaryTree -> (Integer, BinaryTree)
        deleteRightmost (Branch i l Leaf) = (i, l)
        deleteRightmost (Branch i l r)    = let (x, r') = deleteRightmost r
                                             in (x, Branch i l r')

-- an arbitrary generator for BSTs
-- check that this works using:
-- let prop_searchTrees = forAll searchTrees isBST
-- quickCheck prop_searchTrees
searchTrees :: Gen BinaryTree
searchTrees = sized searchTrees'
  where
    searchTrees' 0 = return Leaf
    searchTrees' n = do
      v <- (arbitrary :: Gen Integer)
      fmap (insert v) (searchTrees' $ n - 1)

-- implements a function that satisfies prop_mysteryPred_[1-2]
-- inspection of these properties reveals:
-- this is an "isInTree" function
mysteryPred :: Integer -> BinaryTree -> Bool
mysteryPred x Leaf = False
mysteryPred x (Branch y t1 t2) =
  if x == y
  then True
  else (mysteryPred x t1) || (mysteryPred x t2)

prop_mysteryPred_1 integer = forAll searchTrees $ \tree -> mysteryPred integer (insert integer tree)

prop_mysteryPred_2 integer = forAll searchTrees $ \tree -> not (mysteryPred integer (deleteAll integer tree))

-- implements a function that satisfies prop_mysterious_[1-2]
-- inspection of these properties reveals:
-- this function converts a sorted BST into a sorted list
mysterious :: BinaryTree -> [Integer]
mysterious Leaf = []
mysterious (Branch x t1 t2) = mysterious t1 ++ [x] ++ mysterious t2

prop_mysterious_1 integer = forAll searchTrees $ \tree -> mysteryPred integer tree == (integer `elem` mysterious tree)

prop_mysterious_2 = forAll searchTrees $ isSorted . mysterious

-- predicate that checks whether a list of Integer is sorted
isSorted :: [Integer] -> Bool
isSorted (x:y:rest) = x <= y && isSorted (y:rest)
isSorted _ = True

-- `nub` is a function that removes duplicates from a sorted list
-- check this works using:
-- let prop_sortedListsWithoutDuplicates_1 = forAll sortedListsWithoutDuplicates isSorted
-- let prop_sortedListsWithoutDuplicates_2 = forAll sortedListsWithoutDuplicates $ \x -> x == nub x
-- quickCheck prop_sortedListsWithoutDuplicates_[1-2]
sortedListsWithoutDuplicates :: Gen [Integer]
sortedListsWithoutDuplicates = fmap (nub . sort) arbitrary

-- implements a function that satisfies prop_astonishing_[1-3]
-- inspection of these properties reveals:
-- this function converts a list of Integer into a balanced BST
astonishing :: [Integer] -> BinaryTree
astonishing [] = Leaf
astonishing xs = Branch mid (astonishing first) (astonishing second')
  where
    midway = length xs `quot` 2 -- finds middle index
    mid = xs !! midway -- used to insert median value at root
    (first, second) = splitAt midway xs -- splits list in half
    second' = tail second -- removes median value from 2nd half

prop_astonishing_1 = forAll sortedListsWithoutDuplicates $ isBST . astonishing

prop_astonishing_2 = forAll sortedListsWithoutDuplicates $ isBalanced . astonishing

prop_astonishing_3 = forAll sortedListsWithoutDuplicates $ \ integers -> mysterious (astonishing integers) == integers

-- predicate that returns True if a BinaryTree is balanced
isBalanced :: BinaryTree -> Bool
isBalanced Leaf = True
isBalanced (Branch v l r) = abs (height l - height r) <= 1 && isBalanced l && isBalanced r
  where
    height Leaf = 0
    height (Branch v l r) = 1 + max (height l) (height r)
