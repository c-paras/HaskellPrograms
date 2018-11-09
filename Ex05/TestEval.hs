#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the evaluation of complex propositional formulae.
-}

module TestEval where

import Ex05

main :: IO ()
main = do

  let e = [("a", True), ("b", False), ("c", True), ("d", False)]

  -- truth values
  assert $ eval PTrue e == True
  assert $ eval PFalse e == False

  -- simple variable lookup
  assert $ eval a e == True
  assert $ eval b e == False
  assert $ eval c e == True
  assert $ eval d e == False

  -- truth table for conjunction
  assert $ eval (a /\ c) e == True
  assert $ eval (a /\ b) e == False
  assert $ eval (b /\ a) e == False
  assert $ eval (b /\ d) e == False

  -- truth table for disjunction
  assert $ eval (a \/ c) e == True
  assert $ eval (a \/ b) e == True
  assert $ eval (b \/ a) e == True
  assert $ eval (b \/ d) e == False

  -- truth table for implication
  assert $ eval (a ~> c) e == True
  assert $ eval (a ~> b) e == False
  assert $ eval (b ~> a) e == True
  assert $ eval (b ~> b) e == True

  -- simple compound formulae
  assert $ eval (a /\ b /\ d) e == False
  assert $ eval (a \/ b \/ d) e == True
  assert $ eval (a /\ b ~> d) e == True
  assert $ eval (a ~> b /\ d) e == False
  assert $ eval (a \/ b ~> d) e == False
  assert $ eval (a ~> b \/ d) e == False
  assert $ eval (b /\ d ~> a) e == True
  assert $ eval (b ~> d /\ b) e == True
  assert $ eval (a \/ b ~> c) e == True
  assert $ eval (b ~> a \/ d) e == True

  -- complex compound formulae
  assert $ eval (a /\ c ~> b /\ a) e == False
  assert $ eval (a /\ b /\ c ~> d) e == True
  assert $ eval (b \/ a ~> c \/ d) e == True
  assert $ eval (b \/ a \/ d ~> c) e == True
  assert $ eval (b ~> a \/ d \/ b) e == True
  assert $ eval (a ~> c /\ a /\ b) e == False
  assert $ eval (b \/ a \/ d ~> b) e == False
  assert $ eval (a /\ b /\ c ~> a) e == True

-- displays truth value of a boolean expression
-- if test passes, this should print "True"
assert :: Bool -> IO ()
assert e = do
  let a = show e
  putStrLn a
