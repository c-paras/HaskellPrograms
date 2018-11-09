#!/usr/bin/runhaskell

{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Tests the printing of C-like strings.
-}

module Main where

import Ex07

main :: IO ()
main = do
  let a = L "Hello " $ S $ L "! You are " $ I $ L " years old!" $ X
  putStrLn $ show $ printf a "bob" 14

  let b = S $ S $ S $ I $ I $ I $ X
  putStrLn $ show $ printf b "hello" " " "world " (-3) 0 26

  let c = L "Hello " $ L "World" $ X
  putStrLn $ show $ printf c

  let d = L "this" $ L " is" $ L " a" $ L " test" $ X
  putStrLn $ show $ printf d

  let e = L "Enter a number: " $ I $ L " and a string: " $ S $ X
  putStrLn $ show $ printf e 200 "ok"

  let f = L "integers: " $ I $ L " " $ I $ L " " $ I $ X
  putStrLn $ show $ printf f 3 4 5

  let g = L "strings: " $ S $ L " " $ S $ L " " $ S $ X
  putStrLn $ show $ printf g "three" "four" "five"

  let h = L "easy test case" $ X
  putStrLn $ show $ printf h

  let h = S $ X
  putStrLn $ show $ printf h "only one string"

  let i = I $ X
  putStrLn $ show $ printf i 1
