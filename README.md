# Haskell-AD

Automatic differentiation haskell 구현체

## example

```haskell
import Numeric.AD.Computation
import Numeric.AD.Reverse

main = do
    let x = Var "x"
    let y = Var "y"
    let f = x ** 2 + y ** 2 
    let v = fromList [(x, 3), (y, 4)]
    let g = grad f v

    print g -- [("x",6.0),("y",8.0)]
```