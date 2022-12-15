{-# LANGUAGE FlexibleInstances #-}
module Datum where

-- GHC
import Data.Maybe (listToMaybe)
import Control.Monad.State
import Control.Monad.Except

-- ./prov
import Interface

import Prelude hiding (const, fail)

--------------------------------------------------------------------------------
-- The MiniScheme Expression AST.
-- We refer to all MiniScheme Expressions as Datum.

data Datum =
  Num Int
  | Const Bool
  | Symbol String
  | Nil
  | Cons Datum Datum
  | PrimOp String ([Datum] -> Result Datum)
  | Lambda ([Datum] -> Result Datum)

instance SchemeData Datum where
  symbol = Symbol
  number = Num . fromIntegral
  boolean = Const
  cons = Cons
  nil = Nil
  proper = foldr Cons Nil
  improper xs end = foldr Cons end xs

instance Eq Datum where
  Num i == Num j           = i == j
  Symbol s == Symbol t     = s == t
  Nil == Nil               = True
  Const b == Const c       = b == c
  Cons d1 d2 == Cons e1 e2 = d1 == e1 && d2 == e2
  PrimOp s _ == PrimOp t _ = s == t
  Lambda _   == Lambda _   = False
  _ == _                   = False

instance Show Datum where
  show (Num i)       = show i
  show (Symbol s)    = s
  show Nil           = "nil"
  show (Const True)  = "#t"
  show (Const False) = "#f"
  show (Cons d1 d2)  = "(Cons " ++ show d1 ++ " " ++ show d2 ++ ")"
  show (PrimOp s _)  = "<PrimOp: " ++ s ++ ">"
  show (Lambda s)    = "<Lambda>"

  


-- =============================================================================
-- Results, Errors, & Error handling
-- =============================================================================

-- We move away from the `Result` type of the P1 solution to monads.  The idea
-- is the same: A Result told us either something good happened (the `Datum`
-- evaluated), or something bad happened (we got an `Error`).
--
-- We are abstracting out this behavior by using the more general framework of
-- monad transformers. We instead create a monad that (i) handles state, e.g.,
-- `get` and `put` operations (ii) handles exceptions, e.g., the "Bad" errors
-- that happen.
--
-- Doing this provides the following benefits:

-- 1. Writing our code monadically saves us the headache of perpetually pattern
--    matching and case splitting on the results of evaluation. In a Monad, we
--    can simply ask for the result of evaluating subdatum. If it fails, the
--    monad will handle it. So, we may focus only on the logic we care about.
-- 2. We can use `do` notation to do the above.
-- 3. The monad is polymorphic over *any* return type. In particular, we are
--    able to return, e.g., pairs of Datum rather than just one Datum in an
--    auxilliary function (like when getting the cases when evaluating `cond`).


data Error =  Error String

type Env = [[(String, Datum)]]
type Result a = StateT Env (Either Error) a
    
instance Show Error where
  show (Error s) = "Error: " ++ show s

instance MonadFail (Either Error)
    where fail s = Left (Error s)

--------------------------------------------------------------------------------
-- Monadic Stateful Helpers

lookupEnv :: String -> Result Datum
lookupEnv s = do
  x <- gets (lookupNested s)
  case x of
    Nothing -> notInEnv s
    Just v  -> return v
  where lookupNested :: Eq a => a -> [[(a, b)]] -> Maybe b
        lookupNested str [] = Nothing
        lookupNested str (x:xs) = lookup str x

assign :: String -> Datum -> Result ()
assign s d = modify (\env -> (assign_helper s d (head env)) : env)
  where assign_helper :: String -> Datum -> [(String, Datum)] -> [(String, Datum)]
        assign_helper sym val curEnv = (sym, val) : curEnv

-- Temporarily extend the environment
-- TODO: This probably needs to me where the pop/push functionality of the environment stack will go.
temporary :: Result a -> Result a
temporary m = do
  old <- get
  result <- m
  put old
  return result
    
-- --------------------------------------------------------------------------------
-- -- smart constructors, which take the regular old values (Int, String, Bool)
-- -- and build `Result`s rather than `Datum`. These are purely for convenience.

const :: Bool -> Result Datum
const b = return (Const b)

num :: Int -> Result Datum
num b = return (Num b)

cons :: Datum -> Datum -> Result Datum
cons x y = return (Cons x y)

true, false, nil :: Result Datum
true     = const True
false    = const False
nil      = return Nil

symbol :: String -> Result Datum
symbol s  = return (Symbol s)

lambda :: ([Datum] -> Result Datum) -> Result Datum
lambda l = return (Lambda l)

-- =============================================================================
-- Projective destructors ...
-- =============================================================================
-- These destructors 'project' out of the Datum type into Maybe a.

getNum :: Datum -> Maybe Int
getNum (Num n) = Just n
getNum _       = Nothing

getBool :: Datum -> Maybe Bool
getBool (Const b) = Just b
getBool _         = Nothing

getSymbol :: Datum -> Maybe String
getSymbol (Symbol s) = Just s
getSymbol _          = Nothing

-- =============================================================================
-- List destructors & helpers.
-- =============================================================================

-- The inverse of proper: Take some `datum` and flatten to a list. For example,
-- `(e1 e2 ... en Nil)` is turned into the list [e1, e2, ... , en] :: [Datum].
flatten :: Datum -> [Datum]  
flatten (Cons l r) = l : flatten r
flatten x          = [x]

-- Sometimes we just want to strip Nil from the list (usually, the end of the list).
noNull :: [Datum] -> [Datum]
noNull = filter (\x -> x /= Nil)

-- Is this list of Datum proper? It is if it has `Nil` at the end.
isProper :: [Datum] -> Bool
isProper xs = listToMaybe (reverse xs) == Just Nil

-- Is this list of Datum improper? It is if it does not have `Nil` at the end.
isImproper :: [Datum] -> Bool
isImproper xs = case listToMaybe (reverse xs) of
                  Just Nil -> False
                  Just _   -> True
                  Nothing  -> False

-- =============================================================================
-- Error constructors & error helpers
-- =============================================================================

notImplemented :: String -> Result ()
notImplemented s = fail ("Feature not implemented: " ++ s)

bad :: Result a
bad = fail "Something has gone wrong and I'm not sure why."

argsBorked, invalidArg :: String -> [Datum] -> Result a
argsBorked s xs =
  fail ("Either not enough, or, too many arguments given to "
        ++ s ++ ". Arguments Given: " ++ show xs)
invalidArg s xs = 
  fail ("Invalid argument(s) supplied to "
       ++ s ++ ": " ++ show xs)

notInEnv :: String -> Result a
notInEnv s = fail ("Tried to look up " ++ s ++ " in environment, but couldn't find it.")
