{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Defines vectors as lists parameterized by their length.
-}

module Vec where

import Data.Proxy
import Data.Type.Equality

{-
1. Mathematical logic is a formal system for deducing valid conclusions
from known (or assumed) premises. There are many equivalences between
mathematical logic and programming languages. Model theory corresponds
to the denotational semantics of a program and proof theory corresponds
to the operational semantics.

Propositional logic involves logical formulas that do not rely on
propositional variables, e.g. "The sum of all squares of even numbers
is an even number."

Predicate logic (first-order logic) involves logical formulas with
quantified variables. The formula may be satisfied for a particular set
of instantiations of the variables, e.g. "x^2 > x".
-}

{-
2. Static type checking is more efficient than dynamic typing since
ill-typed programs can be dealt with at compile-time rather than at
runtime. Programming errors are much more difficult to propagate if
static type checking is used. Type errors detected at compile-type
are typically more informative of the error than failing unit tests.
-}

{-
3. The type family computes the type of the printf function given a
list of types in the format string. The type family uses the types in
the list to create a function type for printf so that the right number
of arguments (and types) is assured.
-}

-- 4: zipWith function over vectors:
zipWithV :: (a -> b -> c) -> Vec n a -> Vec m b -> Vec (Min n m) c
zipWithV f VNil xs = VNil
zipWithV f xs VNil = VNil
zipWithV f (VCons x xs) (VCons y ys) = VCons (f x y) (zipWithV f xs ys)

-- 5: replicate function over vectors:
replicateV :: SNat n -> a -> Vec n a
replicateV (Zero) _     = VNil
replicateV (Succ n) val = append (VCons val VNil) (replicateV n val)

-- 6: takeWhile function over vectors?
-- this is not possible since the length
-- of the resulting vector is not known statically
-- here's the definition over lists:
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' fn []     = []
takeWhile' fn (x:xs) =
  if fn x == True
  then [x] ++ takeWhile' fn xs
  else []

-- peano definition of natural numbers
data Nat
  = Z
  | S Nat

-- singleton natural numbers
data SNat (n :: Nat) where
  Zero :: SNat Z
  Succ :: SNat n -> SNat (S n)
deriving instance Show (SNat n)

-- addition of naturals
type family (+) (n :: Nat) (m :: Nat) :: Nat
type instance Z + m     = m
type instance (S n) + m = S (n + m)

-- minimum of two naturals
type family Min (n :: Nat) (m :: Nat) :: Nat
type instance Min Z m         = Z
type instance Min n Z         = Z
type instance Min (S n) (S m) = S (Min n m)

-- vector definition (list parameterized by its length)
data Vec (n :: Nat) a where
  VNil :: Vec Z a
  VCons :: a -> Vec n a -> Vec (S n) a

-- map function over vectors
mapV :: (a -> b) -> Vec n a -> Vec n b
mapV f VNil         = VNil
mapV f (VCons x xs) = VCons (f x) (mapV f xs)

-- append function over vectors
append :: Vec n a -> Vec m a -> Vec (n + m) a
append VNil xs         = xs
append (VCons x xs) ys = VCons x (append xs ys)

-- take function over vectors
takeV :: SNat n -> Vec m a -> Vec n a
takeV Zero xs               = VNil
takeV (Succ n) (VCons x xs) = VCons x (takeV n xs)
