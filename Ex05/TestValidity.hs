#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the checking of satisfiability of propositional formulae.
-}

module TestValidity where

import Ex05

main :: IO ()
main = do

  -- truth values
  assert $ isValid PTrue == True
  assert $ isValid PFalse == False

  -- singleton propositional variable ought to be satisfiable
  assert $ isValid a == True

  -- conjunction, disjunction and implication ought to be satisfiable
  assert $ isValid (a /\ b) == True
  assert $ isValid (a \/ b) == True
  assert $ isValid (a ~> b) == True

  -- unsatisfiable propositions
  assert $ isValid (true /\ false) == False
  assert $ isValid (false /\ true) == False
  assert $ isValid (a /\ false) == False
  assert $ isValid (false /\ a) == False
  assert $ isValid (a /\ b /\ false /\ c /\ d) == False
  assert $ isValid (true ~> b /\ false) == False
  assert $ isValid (a \/ true \/ c ~> false) == False
  assert $ isValid (a \/ true ~> a /\ false) == False

  -- satisfiable propositions
  assert $ isValid (true /\ true) == True
  assert $ isValid (false \/ true) == True
  assert $ isValid (a \/ false) == True
  assert $ isValid (false \/ a) == True
  assert $ isValid (a /\ b /\ c) == True
  assert $ isValid (a \/ b \/ c) == True
  assert $ isValid (a /\ d /\ true) == True
  assert $ isValid (a ~> b) == True

  -- many propositional variables
  let e = PVar "e"
  let f = PVar "f"
  let g = PVar "g"
  let h = PVar "h"
  assert $ isValid (a /\ b /\ c /\ d /\ e /\ f /\ g /\ h) == True
  assert $ isValid (a \/ b \/ c \/ d \/ e \/ f \/ g \/ h) == True
  assert $ isValid (a /\ b /\ c /\ d ~> e /\ f /\ g /\ h) == True
  assert $ isValid (a \/ b \/ c \/ d ~> e \/ f \/ g \/ h) == True

-- displays truth value of a boolean expression
-- if test passes, this should print "True"
assert :: Bool -> IO ()
assert e = do
  let a = show e
  putStrLn a
