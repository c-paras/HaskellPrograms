{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Implements operations on vectors using DataKinds extensions.
-}

module Ex08 where

-- value-level definition of the natural numbers
data Nat
  = Z
  | S Nat

-- type-level defintion of addition of natural numbers
type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z     + m = m
type instance (S n) + m = S (n + m)

-- type-level defintion of multiplication of natural numbers
type family (*) (n :: Nat) (m :: Nat) :: Nat
type instance Z * m     = Z
type instance (S n) * m = m + (n * m) -- since addition is commutative

-- a vector is a list whose length is tracked by a type index of kind Nat
data Vec (n :: Nat) a where
  VNil  :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a -- length increases by 1

deriving instance Show a => Show (Vec n a)
deriving instance Eq a   => Eq   (Vec n a)

-- extracts the first element of a vector
-- e.g. headV (VCons 2 VNil) will work whereas headV VNil won't
-- tests:
{-
headV VNil
headV $ VCons 2 VNil
headV $ VCons 1 $ VCons 9 VNil
headV $ VCons 6 $ VCons 7 $ VCons 4 VNil
headV $ VCons 3 $ VCons 2 $ VCons 5 $ VCons 8 VNil
-}
headV :: Vec (S n) a -> a -- ensure only a non-empty list compiles
headV (VCons x xs) = x -- pattern-match only on VCons

-- appends two vectors
-- e.g. appendV (VCons 4 VNil) (VCons 5 VNil)
appendV :: Vec n a -> Vec m a -> Vec (n + m) a
appendV VNil         ys = ys
appendV (VCons x xs) ys = VCons x (appendV xs ys)

-- concatenates a vector of vectors into one vector
-- e.g. concatV $ VCons (VCons 4 VNil) VNil
-- tests:
{-
concatV $ VNil
concatV $ VCons VNil VNil
concatV $ VCons (VCons 4 VNil) VNil
concatV $ VCons (VCons 3 (VCons 8 VNil)) VNil
concatV $ VCons (VCons (VCons 6 VNil) VNil) VNil
-}
concatV :: Vec n (Vec m a) -> Vec (n * m) a
concatV VNil          = VNil
concatV (VCons xs ys) = appendV xs (concatV ys)

-- concatenates a list of lists into one list
conc :: [[a]] -> [a]
conc []     = []
conc (x:xs) = x ++ conc xs

-- conc constructs a new list (using the cons constructor)
-- with x followed by xs & ys appended to each other
-- concatV does the same but with vectors instead of lists
