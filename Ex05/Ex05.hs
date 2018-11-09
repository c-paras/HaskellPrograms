{-
   Copyright (C) 2017 Costa Paraskevopoulos.
   Evaluates propositional formulae and performs satisfiability checking.
-}

module Ex05 where

data PropFormula
  = PTrue
  | PFalse
  | PAnd  PropFormula PropFormula
  | POr   PropFormula PropFormula
  | PImpl PropFormula PropFormula
  | PVar  String
  deriving (Show, Eq)

-- environment is a list of variable names paired with their values
type Env = [(String, Bool)]

-- infix operators to conveniently construct PropFormula terms
infixr 5 /\
infixr 4 \/
infixr 3 ~>

(/\) :: PropFormula -> PropFormula -> PropFormula
(/\) = PAnd

(\/) :: PropFormula -> PropFormula -> PropFormula
(\/) = POr

(~>) :: PropFormula -> PropFormula -> PropFormula
(~>) = PImpl

-- defines true and false as PropFormula terms
true, false :: PropFormula
true  = PTrue
false = PFalse

-- defines some propositional variables
a, b, c, d :: PropFormula
a = PVar "a"
b = PVar "b"
c = PVar "c"
d = PVar "d"

-- evaluates a formula without variables
-- returns an error if formula contains a variable
evalSimple :: PropFormula -> Bool
evalSimple PTrue       = True
evalSimple PFalse      = False
evalSimple (PAnd  p q) = evalSimple p && evalSimple q
evalSimple (POr   p q) = evalSimple p || evalSimple q
evalSimple (PImpl p q) = (not (evalSimple p)) || evalSimple q
evalSimple (PVar    v) = error "cannot handle variables"

-- evaluates a formula with variables
eval :: PropFormula -> Env -> Bool
eval PTrue       e = True
eval PFalse      e = False
eval (PAnd  p q) e = eval p e && eval q e
eval (POr   p q) e = eval p e || eval q e
eval (PImpl p q) e = (not (eval p e)) || eval q e
eval (PVar    v) e = checkEnv v e
  where
    -- looksup a variable in the environment, returning its truth value
    checkEnv :: String -> Env -> Bool
    checkEnv s []     = error $ show s ++ " is not in env"
    checkEnv s [x]    = if s == fst x -- redundant case
                        then snd x
                        else error $ show s ++ " is not in env"
    checkEnv s (x:xs) = if s == fst x
                        then snd x
                        else checkEnv s xs

-- checks whether a formula is satisfiable
-- this is a misleading function name since it doesn't check validity
isValid :: PropFormula -> Bool
isValid p = tryAllPermutations p $ createEnv $ freeVars p
  where

    -- assigns False to every variable initially
    createEnv :: [String] -> Env
    createEnv [] = []
    createEnv (x:xs) = (x, False) : (createEnv xs)

    -- tries all possible permutations of truth values
    tryAllPermutations :: PropFormula -> Env -> Bool
    tryAllPermutations p e = if eval p e == True
                       then True
                       else case (getNextPermutation e) of
                              [] -> False -- exhausted all perms
                              e' -> tryAllPermutations p e'

    -- used to cycle thru all permutations of truth values
    -- an empty env is returned when there are no more perms left
    -- assumes the initial env is composed of False
    -- worse case: all perms are tried to reach only True variables
    getNextPermutation :: Env -> Env
    getNextPermutation [] = []
    getNextPermutation e = if allTrue e
                           then [] -- avoids splitAt (-1) (reverse e)
                           else x1 ++ x2'
      where
        -- checks from right to left for first occurance of False
        -- sets that variable to True
        -- sets everything after (i.e. to the right of) that to False
        -- e.g. 00111 beocomes 01000
        -- once 11111 is reached, -1 would be returned as the index
        -- but this is avoided by checking allTrue e above
        i = findFirstFalse (reverse e) 1
        (x2, x1) = splitAt i (reverse e)
        x2' = case (x2) of
               [] -> []
               l -> (fst $ head l, True) : (makeFalse $ tail l)

        -- returns index of first instance of False in list
        findFirstFalse :: Env -> Int -> Int
        findFirstFalse [] i = -1
        findFirstFalse (x:xs) i = if snd x == False
                                  then i
                                  else findFirstFalse xs $ i + 1

        -- checks if all variables are True, prompting termination
        allTrue :: Env -> Bool
        allTrue [] = True
        allTrue (x:xs) = snd x && allTrue xs

        -- sets all variables in the env to False
        makeFalse :: Env -> Env
        makeFalse [] = []
        makeFalse (x:xs) = (fst x, False) : (makeFalse xs)

-- returns list of names of all free variables in a formula
freeVars :: PropFormula -> [String]
freeVars PTrue       = []
freeVars PFalse      = []
freeVars (PAnd  p q) = freeVars p ++ freeVars q
freeVars (POr   p q) = freeVars p ++ freeVars q
freeVars (PImpl p q) = freeVars p ++ freeVars q
freeVars (PVar    v) = [v]
