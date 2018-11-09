{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Defines a C-like printf function using DataKinds extensions.
-}

module Ex07 where

-- format string parameterized by a list of types
data Format (fmt :: [*]) where
  X :: Format '[] -- empty string, i.e. ""

  -- first approximation:
  -- L :: a -> String -> Format '[]
  -- S :: a -> Format '[String]
  -- I :: a -> Format '[Int]
  -- but this discards all the previous types

  L :: String -> Format a -> Format a   -- string literal, e.g. "hello"
  S :: Format a -> Format (String ': a) -- "%s"
  I :: Format a -> Format (Int ': a)    -- "%d"

-- computes the type of printf given a format string
type family FormatArgsThen (fmt :: [*]) (ty :: *) :: *
type instance FormatArgsThen '[]       ty = ty
type instance FormatArgsThen (t ': ts) ty = t -> FormatArgsThen ts ty

-- creates a string suitable for printing
-- by splicing values into the format string
-- based on the 1st arg (a format containing a list of types)
printf :: Format fmt -> FormatArgsThen fmt String
printf fmt = printf' fmt "" -- start with empty string accumulator
  where
    printf' :: Format fmt -> String -> FormatArgsThen fmt String
    printf' X str         = str

    -- append literal string to accumulator
    printf' (L s fmt) str = printf' fmt (str ++ s)

    -- append next arg to printf (should be a string)
    printf' (S fmt) str   = \s -> printf' fmt (str ++ s)

    -- append next arg to printf (should be an int)
    printf' (I fmt) str   = \d -> printf' fmt (str ++ (show d))
