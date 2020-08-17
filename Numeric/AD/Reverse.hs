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
    Multipl (T g g') (T h h') -> trace g v (seed * h') ++ trace h v (seed * g')
    Abs (T g g')              -> trace g v (if g' > 0 then seed else -seed)
    Signum g                  -> trace g v 0
    Divide (T g g') (T h h')  -> trace g v (seed / h') ++ trace h v (seed * g' / h' ** 2)
    Sin (T g g')              -> trace g v (seed * cos g')
    Exp (T g g')              -> trace g v (seed * exp g')
    Sum (Vector g)            -> L.foldl1' (++) $ map (\g' -> trace g' v seed) g

accum :: (Ord a, Floating a) => E a -> V a -> (a, E a)
accum f v = case f of
    Var coord      -> (v M.! coord, f)
    Const x        -> (x, f)
    Negate g       -> let (a, g') = accum g v in (-a, g')
    Add g h        -> let (a, g') = accum g v; (b, h') = accum h v in (a + b, Add g' h')
    Sub g h        -> let (a, g') = accum g v; (b, h') = accum h v in (a - b, Sub g' h')
    Multipl g h    -> let (a, g') = accum g v; (b, h') = accum h v in (a * b, Multipl (T g' a) (T h' b))
    Abs g          -> let (a, g') = accum g v in (abs a, T g' a)
    Signum g       -> let (a, g') = accum g v in (signum a, g')
    Divide g h     -> let (a, g') = accum g v; (b, h') = accum h v in (a / b, Divide (T g' a) (T h' b))
    Sin g          -> let (a, g') = accum g v in (sin a, Sin (T g' a))
    Exp g          -> let (a, g') = accum g v in (exp a, Exp (T g' a))
    Sum (Vector g) -> let (a, g') = unzip $ map (`accum` v) g in (sum a, Sum (Vector g'))

grad :: (Ord a, Floating a) => E a -> V a -> V a
grad f v = L.foldl1' (M.unionWith (+)) $ map (\x -> M.fromList [x]) $ trace (snd $ accum f v) v 1