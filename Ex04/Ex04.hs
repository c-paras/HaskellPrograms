{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Performs simple IO by writing and reading files.
-}

module Ex04 where

import Data.Char
import System.Environment
import System.IO

-- reads a text file and writes out a capitilized version to another file
capitalise :: FilePath -> FilePath -> IO ()
capitalise f g = do
  s <- readFile f
  writeFile g (cap s)
  where

    -- capitalizes input string
    cap :: String -> String
    cap ""     = ""
    cap (x:xs) = (toUpper x) : (cap xs)

-- sums integers separated by newlines in a file and writes the result
-- to another file, both filenames are specified as cmd args
sumFile :: IO ()
sumFile = do
  [f, g] <- getArgs
  s      <- readFile f
  let nums = lines s
  writeFile g $ (show (foldl (+) 0 (toInt nums))) ++ "\n"
  where

    -- converts list of String to list of Integer
    toInt :: [String] -> [Integer]
    toInt [] = []
    toInt (x:xs) = parseInt x : toInt xs

    -- returns 0 if string cannot be parsed (doesn't affect the sum)
    parseInt :: String -> Integer
    parseInt s = case (reads s :: [(Integer, String)]) of
                   [(i, _)] -> i
                   _        -> 0
