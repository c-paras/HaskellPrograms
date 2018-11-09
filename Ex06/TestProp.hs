#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the evaluation of boolean propositions by type-level functions.
-}

module Main where

import Ex06

main :: IO ()
main = do
  -- logical truth values
  putStrLn $ show $ eval PTrue == True
  putStrLn $ show $ eval PFalse == False

  -- logical negation
  putStrLn $ show $ eval (PNot PTrue) == False
  putStrLn $ show $ eval (PNot PFalse) == True

  -- logical conjunction
  putStrLn $ show $ eval (PAnd PTrue PTrue) == True
  putStrLn $ show $ eval (PAnd PTrue PFalse) == False
  putStrLn $ show $ eval (PAnd PFalse PTrue) == False
  putStrLn $ show $ eval (PAnd PFalse PFalse) == False

  -- logical disjunction
  putStrLn $ show $ eval (POr PTrue PTrue) == True
  putStrLn $ show $ eval (POr PTrue PFalse) == True
  putStrLn $ show $ eval (POr PFalse PTrue) == True
  putStrLn $ show $ eval (POr PFalse PFalse) == False

  -- compound propositions
  putStrLn $ show $ eval (PAnd (PAnd PTrue PFalse) PFalse) == False
  putStrLn $ show $ eval (POr (POr PTrue PFalse) PFalse) == True
  putStrLn $ show $ eval (POr (PAnd PTrue PFalse) PFalse) == False
  putStrLn $ show $ eval (PAnd (POr PTrue PFalse) PFalse) == False
  putStrLn $ show $ eval (POr (PAnd PTrue PFalse) PTrue) == True
  putStrLn $ show $ eval (PAnd (POr PTrue PFalse) PTrue) == True

  -- complex propositions
  putStrLn $ show $ eval (PAnd (POr (PNot PTrue) PFalse) PTrue) == False
  putStrLn $ show $ eval (PAnd (POr (PNot PTrue) (PNot PTrue)) PTrue) == False
  putStrLn $ show $ eval (PAnd (POr PFalse (PNot PFalse)) PFalse) == False
  putStrLn $ show $ eval (PAnd (POr PFalse (PNot PFalse)) PTrue) == True
  putStrLn $ show $ eval (POr (PAnd PTrue (PNot PFalse)) PFalse) == True
