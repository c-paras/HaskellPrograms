#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the evaluation of simple propositional formulae.
-}

module TestEvalSimple where

import Ex05

main :: IO ()
main = do

  -- truth values
  assert $ evalSimple PTrue == True
  assert $ evalSimple PFalse == False

  -- truth table for conjunction
  assert $ evalSimple (true /\ true) == True
  assert $ evalSimple (true /\ false) == False
  assert $ evalSimple (false /\ true) == False
  assert $ evalSimple (false /\ false) == False

  -- truth table for disjunction
  assert $ evalSimple (true \/ true) == True
  assert $ evalSimple (true \/ false) == True
  assert $ evalSimple (false \/ true) == True
  assert $ evalSimple (false \/ false) == False

  -- truth table for implication
  assert $ evalSimple (true ~> true) == True
  assert $ evalSimple (true ~> false) == False
  assert $ evalSimple (false ~> true) == True
  assert $ evalSimple (false ~> false) == True

  -- simple compound formulae
  assert $ evalSimple (true /\ false /\ false) == False
  assert $ evalSimple (true \/ false \/ false) == True
  assert $ evalSimple (true /\ false ~> false) == True
  assert $ evalSimple (true ~> false /\ false) == False
  assert $ evalSimple (true \/ false ~> false) == False
  assert $ evalSimple (true ~> false \/ false) == False
  assert $ evalSimple (false /\ false ~> true) == True
  assert $ evalSimple (false ~> false /\ false) == True
  assert $ evalSimple (true \/ false ~> true) == True
  assert $ evalSimple (false ~> true \/ false) == True

  -- complex compound formulae
  assert $ evalSimple (true /\ true ~> false /\ true) == False
  assert $ evalSimple (true /\ false /\ true ~> false) == True
  assert $ evalSimple (false \/ true ~> true \/ false) == True
  assert $ evalSimple (false \/ true \/ false ~> true) == True
  assert $ evalSimple (false ~> true \/ false \/ false) == True
  assert $ evalSimple (true ~> true /\ true /\ false) == False
  assert $ evalSimple (false \/ true \/ false ~> false) == False
  assert $ evalSimple (true /\ false /\ true ~> true) == True

-- displays truth value of a boolean expression
-- if test passes, this should print "True"
assert :: Bool -> IO ()
assert e = do
  let a = show e
  putStrLn a
