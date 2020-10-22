import Numeric.AD.Computation
import Numeric.AD.Reverse

main = do
    let x = Var "x"
    let y = Var "y"
    let f = log x
    let v = fromList [(x, 3), (y, 4)]
    let g = grad f v
    print f
    print v
    print g