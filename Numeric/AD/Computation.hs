module Numeric.AD.Computation 
    (
      E
    , V
    , Coord
    , eval
    ) where

import Data.Map as M

type Coord = String
type V a = M.Map Coord a

data E a = Var     Coord
         | T       (E a) a
         | Vector  [E a]
         | Matrix  [[E a]]
         | Const   a
         | Negate  (E a)
         | Add     (E a) (E a)
         | Sub     (E a) (E a)
         | Multipl (E a) (E a)
         | Abs     (E a)
         | Signum  (E a)
         | Divide  (E a) (E a)
         | Exp     (E a)
         | Sqrt    (E a)
         | Log     (E a)
         | Sin     (E a)
         | Cos     (E a)
         | Tan     (E a)
         | Asin    (E a)
         | Acos    (E a)
         | Atan    (E a)
         | Sinh    (E a)
         | Cosh    (E a)
         | Tanh    (E a)
         | Asinh   (E a)
         | Acosh   (E a)
         | Atanh   (E a)
         | Pow     (E a) (E a)
         | LogBase (E a) (E a)
         | Sum     (E a)
         deriving Show

instance (Floating a) => Num (E a) where
  (+)           = Add
  (-)           = Sub
  (*)           = Multipl
  negate x      = case x of 
      Const x -> Const (-x)
      x       -> Negate x
  abs           = Abs
  signum x      = case x of
    Const x -> Const (signum x)
    x       -> Signum x
  fromInteger x = Const $ fromInteger x

instance (Floating a) => Fractional (E a) where
    (/)            = Divide
    fromRational a = Const (fromRational a)

instance (Floating a) => Floating (E a) where
    pi      = Const pi
    exp     = Exp
    sqrt    = Sqrt
    log     = Log
    sin     = Sin
    cos     = Cos
    tan     = Tan
    asin    = Asin
    acos    = Acos
    atan    = Atan
    sinh    = Sinh
    cosh    = Cosh
    tanh    = Tanh
    asinh   = Asinh
    acosh   = Acosh
    atanh   = Atanh
    (**)    = Pow
    logBase = LogBase

sumV :: Floating a => E a -> E a
sumV a@(Vector _) = Sum a

eval :: Floating a => E a -> V a -> a
eval e v = case e of
    (Var coord)      -> v M.! coord
    (Const v)        -> v
    (Add x y)        -> eval x v + eval y v
    (Sub x y)        -> eval x v - eval y v
    (Negate x)       -> - eval x v
    (Multipl x y)    -> eval x v * eval y v
    (Abs x)          -> abs $ eval x v
    (Signum x)       -> signum $ eval x v
    (Divide x y)     -> eval x v / eval y v
    (Exp x)          -> exp $ eval x v
    (Sqrt x)         -> sqrt $ eval x v
    (Log x)          -> log $ eval x v
    (Sin x)          -> sin $ eval x v
    (Cos x)          -> cos $ eval x v
    (Tan x)          -> tan $ eval x v
    (Asin x)         -> asin $ eval x v
    (Acos x)         -> acos $ eval x v
    (Atan x)         -> atan $ eval x v
    (Sinh x)         -> sinh $ eval x v
    (Cosh x)         -> cosh $ eval x v
    (Tanh x)         -> tanh $ eval x v
    (Asinh x)        -> asinh $ eval x v
    (Acosh x)        -> acosh $ eval x v
    (Atanh x)        -> atanh $ eval x v
    (Pow x y)        -> eval x v ** eval y v
    (LogBase x y)    -> logBase (eval x v) (eval y v)
    (Sum (Vector a)) -> sum $ map (`eval` v) a
