{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Defines GADTs for binary trees and boolean propositions.
-}

module Ex06 where

-- BinaryTree type paramertised by a value and tag (Empty/NonEmpty)
data BinaryTree (a :: Tag) b where

  -- a Leaf is tagged with an Empty kind
  Leaf :: BinaryTree Empty b

  -- a Branch is tagged with a NonEmpty kind
  -- the left child need not have the same tag as the right child
  -- this is why different tags c and c' are used
  Branch :: b -> BinaryTree c b -> BinaryTree c' b -> BinaryTree NonEmpty b

-- tags used to paramterise BinaryTree
data Tag
  = Empty
  | NonEmpty

-- safely returns top element of a BinaryTree
-- a BinaryTree that is Empty will not type check
top :: BinaryTree NonEmpty a -> a
top (Branch v l r) = v

-- boolean proposition type parameterised by its truth value
data BoolProp (a :: Bool) where
  PTrue  :: BoolProp True
  PFalse :: BoolProp False
  PAnd   :: BoolProp a -> BoolProp b -> BoolProp (a && b)
  POr    :: BoolProp a -> BoolProp b -> BoolProp (a || b)
  PNot   :: BoolProp a -> BoolProp (Not a)

-- implements a type family for logical negation
type family Not (a :: Bool) :: Bool
type instance Not True  = False
type instance Not False = True

-- implements a type family for logical conjunction
type family (&&) (a :: Bool) (b :: Bool) :: Bool
type instance (&&) True   True = True
type instance (&&) True  False = False
type instance (&&) False  True = False
type instance (&&) False False = False

-- implements a type family for logical disjunction
type family (||) (a :: Bool) (b :: Bool) :: Bool
type instance (||) True   True = True
type instance (||) True  False = True
type instance (||) False  True = True
type instance (||) False False = False

-- exports type-level information to value-level
class ToRuntimeBool a where
  eval :: a -> Bool

-- exports type-level True to value-level True
instance ToRuntimeBool (BoolProp True) where
  eval _ = True

-- exports type-level False to value-level False
instance ToRuntimeBool (BoolProp False) where
  eval _ = False
