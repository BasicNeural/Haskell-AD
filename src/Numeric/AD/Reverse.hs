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
    Sub g h                   -> trace g v seed ++ trace h v (-seed)
    Multipl (T g gx) (T h hx) -> trace g v (hx * seed) ++ trace h v (gx * seed)
    Abs (T g gx)              -> trace g v (if gx > 0 then seed else -seed)
    Signum g                  -> trace g v 0
    Divide (T g gx) (T h hx)  -> trace g v (seed / hx) ++ trace h v (-gx * seed / hx ** 2)
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

accum :: (Ord a, Floating a) => E a -> V a -> (a, E a)
accum f v = case f of
    Var coord      -> (v M.! coord, f)
    Const x        -> (x, f)
    Negate g       -> let (gx, g') = accum g v in (-gx, Negate g')
    Add g h        -> let (gx, g') = accum g v; (hx, h') = accum h v in (gx + hx, Add g' h')
    Sub g h        -> let (gx, g') = accum g v; (hx, h') = accum h v in (gx - hx, Sub g' h')
    Multipl g h    -> let (gx, g') = accum g v; (hx, h') = accum h v in (gx * hx, Multipl (T g' gx) (T h' hx))
    Abs g          -> let (gx, g') = accum g v in (abs gx, T g' gx)
    Signum g       -> let (gx, g') = accum g v in (signum gx, g')
    Divide g h     -> let (gx, g') = accum g v; (hx, h') = accum h v in (gx / hx, Divide (T g' gx) (T h' hx))
    Sin g          -> let (gx, g') = accum g v in (sin gx, Sin (T g' gx))
    Cos g          -> let (gx, g') = accum g v in (cos gx, Cos (T g' gx))
    Tan g          -> let (gx, g') = accum g v in (tan gx, Tan (T g' gx))
    Asin g         -> let (gx, g') = accum g v in (asin gx, Asin (T g' gx))
    Acos g         -> let (gx, g') = accum g v in (acos gx, Acos (T g' gx))
    Atan g         -> let (gx, g') = accum g v in (atan gx, Atan (T g' gx))
    Sinh g         -> let (gx, g') = accum g v in (sinh gx, Sinh (T g' gx))
    Cosh g         -> let (gx, g') = accum g v in (cosh gx, Cosh (T g' gx))
    Tanh g         -> let (gx, g') = accum g v in (tanh gx, Tanh (T g' gx))
    Asinh g        -> let (gx, g') = accum g v in (asinh gx, Asinh (T g' gx))
    Acosh g        -> let (gx, g') = accum g v in (acosh gx, Acos (T g' gx))
    Atanh g        -> let (gx, g') = accum g v in (atanh gx, Atanh (T g' gx))
    Exp g          -> let (gx, g') = accum g v in (exp gx, Exp (T g' gx))
    Pow g h        -> let (gx, g') = accum g v; (hx, h') = accum h v in (gx ** hx, Pow (T g' gx) (T h' hx))
    Sqrt g         -> let (gx, g') = accum g v in (sqrt gx, Sqrt (T g' gx))
    Log g          -> let (gx, g') = accum g v in (log gx, Log (T g' gx))

grad :: (Ord a, Floating a) => E a -> V a -> V a
grad f v = L.foldl1' (M.unionWith (+)) $ map (\x -> M.fromList [x]) $ trace (snd $ accum f v) v 1
