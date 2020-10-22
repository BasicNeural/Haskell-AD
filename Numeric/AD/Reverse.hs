module Numeric.AD.Reverse where

import Numeric.AD.Computation
import qualified Data.List as L
import qualified Data.Map as M

trace :: (Ord a, Floating a) => E a -> V a -> a -> [(Coord, a)]
trace f v seed = case f of
    Var coord                 -> [(coord, seed)]
    Const x                   -> []
    Negate g                  -> trace g v (-seed)
    Add g h                   -> trace g v seed ++ trace h v seed
    Sub g h                   -> trace g v seed ++ trace h v seed
    Multipl (T g gx) (T h hx) -> trace g v (hx * seed) ++ trace h v (gx * seed)
    Abs (T g gx)              -> trace g v (if gx > 0 then seed else -seed)
    Signum g                  -> trace g v 0
    Divide (T g gx) (T h hx)  -> trace g v (seed / hx) ++ trace h v (-gx / hx ** 2 * seed)
    Sin (T g gx)              -> trace g v (cos gx * seed)
    Cos (T g gx)              -> trace g v (-sin gx * seed)
    Tan (T g gx)              -> trace g v (seed / cos gx ** 2)
    Asin (T g gx)             -> trace g v (seed / sqrt (1 - gx ** 2))
    Acos (T g gx)             -> trace g v (-seed / sqrt (1 - gx ** 2))
    Atan (T g gx)             -> trace g v (seed / (1 + gx ** 2))
    Sinh (T g gx)             -> trace g v (cosh gx * seed)
    Cosh (T g gx)             -> trace g v (sinh gx * seed)
    Tanh (T g gx)             -> trace g v (seed / cosh gx ** 2)
    Asinh (T g gx)            -> trace g v (seed / sqrt (gx ** 2 + 1))
    Acosh (T g gx)            -> trace g v (seed / sqrt (gx ** 2 - 1))
    Atanh (T g gx)            -> trace g v (seed / (1 - gx ** 2))
    Exp (T g gx)              -> trace g v (exp gx * seed)
    Pow (T g gx) (T h hx)     -> trace g v (hx * gx ** (hx - 1) * seed) ++ trace h v (gx ** hx * log gx * seed)
    LogBase (T g gx) (T h hx) -> trace g v (- log gx / (hx * log gx ** 2)) ++ trace h v (seed / (hx * log gx))
    Sqrt (T g gx)             -> trace g v (seed / (2 * sqrt gx))
    Log (T g gx)              -> trace g v (seed / gx)
    Sum (Vector g)            -> L.foldl1' (++) $ map (\gx -> trace gx v seed) g

accum :: (Ord a, Floating a) => E a -> V a -> (a, E a)
accum f v = case f of
    Var coord      -> (v M.! coord, f)
    Const x        -> (x, f)
    Negate g       -> let (a, gx) = accum g v in (-a, gx)
    Add g h        -> let (a, gx) = accum g v; (b, hx) = accum h v in (a + b, Add gx hx)
    Sub g h        -> let (a, gx) = accum g v; (b, hx) = accum h v in (a - b, Sub gx hx)
    Multipl g h    -> let (a, gx) = accum g v; (b, hx) = accum h v in (a * b, Multipl (T gx a) (T hx b))
    Abs g          -> let (a, gx) = accum g v in (abs a, T gx a)
    Signum g       -> let (a, gx) = accum g v in (signum a, gx)
    Divide g h     -> let (a, gx) = accum g v; (b, hx) = accum h v in (a / b, Divide (T gx a) (T hx b))
    Sin g          -> let (a, gx) = accum g v in (sin a, Sin (T gx a))
    Cos g          -> let (a, gx) = accum g v in (cos a, Cos (T gx a))
    Tan g          -> let (a, gx) = accum g v in (tan a, Tan (T gx a))
    Asin g         -> let (a, gx) = accum g v in (asin a, Asin (T gx a))
    Acos g         -> let (a, gx) = accum g v in (acos a, Acos (T gx a))
    Atan g         -> let (a, gx) = accum g v in (atan a, Atan (T gx a))
    Sinh g         -> let (a, gx) = accum g v in (sinh a, Sinh (T gx a))
    Cosh g         -> let (a, gx) = accum g v in (cosh a, Cosh (T gx a))
    Tanh g         -> let (a, gx) = accum g v in (tanh a, Tanh (T gx a))
    Asinh g        -> let (a, gx) = accum g v in (asinh a, Asinh (T gx a))
    Acosh g        -> let (a, gx) = accum g v in (acosh a, Acos (T gx a))
    Atanh g        -> let (a, gx) = accum g v in (atanh a, Atanh (T gx a))
    Exp g          -> let (a, gx) = accum g v in (exp a, Exp (T gx a))
    Pow g h        -> let (a, gx) = accum g v; (b, hx) = accum h v in (a ** b, Pow (T gx a) (T hx b))
    Sqrt g         -> let (a, gx) = accum g v in (sqrt a, Sqrt (T gx a))
    Log g          -> let (a, gx) = accum g v in (log a, Log (T gx a))
    Sum (Vector g) -> let (a, gx) = unzip $ map (`accum` v) g in (sum a, Sum (Vector gx))

grad :: (Ord a, Floating a) => E a -> V a -> V a
grad f v = L.foldl1' (M.unionWith (+)) $ map (\x -> M.fromList [x]) $ trace (snd $ accum f v) v 1
