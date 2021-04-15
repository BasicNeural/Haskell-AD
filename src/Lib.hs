{-# LANGUAGE TupleSections #-}
module Lib
    ( someFunc
    ) where

import Numeric.AD.Computation as C ( E(Var), fromList, cross, eval, toLaTex )
import Numeric.AD.Reverse
import Data.List as L
import Data.Map as M
import System.Random

varMatrix :: [Char] -> Int -> Int -> [[E a]]
varMatrix x m n = fmap (\i -> fmap (\j -> Var $ x ++ "_{" ++ (show i ++ show j) ++ "}") [1..n]) [1..m]

sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

loss :: Floating a => (b -> a) -> [b] -> [a] -> a
loss h x y = foldl1' (+) (zipWith (\x y -> y * log (h x) + (1 - y) * log (1 - h x)) x y) / (-m)
    where m = fromIntegral $ length y

gd v c 0 = v
gd v c n = gd v' c (n - 1)
    where
        g = grad c v
        v' = M.unionWith (\x y -> x - 0.1 * y) v g

someFunc :: IO ()
someFunc = do
    let w1 = varMatrix "w^1" 2 2
    let b1 = [Var "b^1_1", Var "b1_2"]
    let w2 = varMatrix "w^2" 1 2
    let b2 = [Var "b^2_1"]
    let h x = head $ fmap sigmoid (cross w2 (fmap sigmoid (cross w1 x + b1)) + b2)
    let input = [[0,0],[0,1],[1,0],[1,1]]
    let output = [0,1,1,0]
    let c = loss h input output
    gen <- getStdGen

    let v = C.fromList $ zip (concat w1 ++ concat w2 ++ b1 ++ b2) (randoms gen :: [Float])

    let v' = gd v c 10000

    putStrLn $ toLaTex $ sigmoid (Var "x")

    putStrLn $ toLaTex $ h [Var "x_1", Var "x_2"]

    mapM_ (\x -> print $ eval (h x) v) input
    putChar '\n'
    mapM_ (\x -> print $ eval (h x) v') input
