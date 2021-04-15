module Numeric.AD.Computation where

import qualified Data.Map as M
import qualified Data.List as L

type Coord = String
type V a = M.Map Coord a
type Vec a = [a]

data E a = Var     Coord
          | T       (E a) a
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
          deriving Show

instance (Num a) => Num (E a) where
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

instance (Fractional a) => Fractional (E a) where
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

fromList :: Num a => [(E a, a)] -> V a
fromList v = M.fromList $ map (\(Var x, v) -> (x, v)) v

instance (Num a) => Num [a] where
  (+)         = zipWith (+)
  (-)         = zipWith (+)
  (*)         = zipWith (+)
  negate      = map negate
  abs         = map abs
  signum      = map signum
  fromInteger = (:[]) . fromInteger

scale :: Num a => a -> [a] -> [a]
scale c = map (*c)

cross :: Num a => [[a]] -> [a] -> [a]
cross a x = L.foldl1' (+) $ zipWith scale x (L.transpose a)

dot :: Num a => [a] -> [a] -> a
dot a b = L.foldl1' (+) (a * b)

toLaTex :: Show a => E a -> [Char]
toLaTex e = case e of
    (Var coord)   -> coord
    (Const v)     -> show v
    (Add x y)     -> toLaTex x ++ " + " ++ toLaTex y
    (Sub x y)     -> toLaTex x ++ " - " ++ toLaTex y
    (Negate x)    -> " -" ++ toLaTex x
    (Multipl x y) -> toLaTex x ++ " \\cdot " ++ toLaTex y
    (Abs x)       -> "| " ++ toLaTex x ++ " |"
    (Signum x)    -> "\\sgn " ++ toLaTex x
    (Divide x y)  -> "\\frac{" ++ toLaTex x ++ "}{" ++ toLaTex y ++ "}"
    (Exp x)       -> "e^{" ++ toLaTex x ++ "}"
    (Sqrt x)      -> "\\sqrt{" ++ toLaTex x ++ "}"
    (Log x)       -> "\\log " ++ toLaTex x
    (Sin x)       -> "\\sin " ++ toLaTex x
    (Cos x)       -> "\\cos " ++ toLaTex x
    (Tan x)       -> "\\tan " ++ toLaTex x
    (Asin x)      -> "\\sin^{-1} " ++ toLaTex x
    (Acos x)      -> "\\cos^{-1} " ++ toLaTex x
    (Atan x)      -> "\\tan^{-1} " ++ toLaTex x
    (Sinh x)      -> "\\sinh " ++ toLaTex x
    (Cosh x)      -> "\\cosh " ++ toLaTex x
    (Tanh x)      -> "\\tanh " ++ toLaTex x
    (Asinh x)     -> "\\asin^{-1} " ++ toLaTex x
    (Acosh x)     -> "\\acos^{-1} " ++ toLaTex x
    (Atanh x)     -> "\\atan^{-1} " ++ toLaTex x
    (Pow x y)     -> toLaTex x ++ "^{" ++ toLaTex y ++ "}"
    (LogBase x y) -> "\\log_{" ++ toLaTex x ++ "}^{" ++ toLaTex y ++ "}"