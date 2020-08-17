module Data.Dual 
        (
          Dual((:+:))
        , realPart
        , nilPart
        ) where

infix 6 :+:
data Dual a = !a :+: !a
    deriving ( Eq
             , Ord
             , Show
             )

realPart :: Dual a -> a
realPart (x :+: _) = x

nilPart :: Dual a -> a
nilPart (_ :+: y) = y

instance (RealFloat a) => Num (Dual a) where
    (x:+:y) + (x':+:y') = (x+x') :+: (y+y')
    (x:+:y) - (x':+:y') = (x-x') :+: (y-y')
    (x:+:y) * (x':+:y') = (x*x') :+: (x*y'+x'*y)
    negate (x:+:y)      = negate x :+: negate y
    abs (x:+:y)         = abs x :+: (y * signum x)
    signum (x:+:y)      = signum x :+: 0
    fromInteger n       = fromInteger n :+: 0

instance (RealFloat a) => Fractional (Dual a) where
    (x:+:y) / (x':+:y') = (x/x') :+: ((x'*y-x*y')/x'**2)
    fromRational a      = fromRational a :+: 0

instance (RealFloat a) => Floating (Dual a) where
    pi                        = pi :+: 0
    exp (x:+:y)               = exp x :+: (y * exp x)
    sqrt (x:+:y)              = sqrt x :+: (y / (2 * sqrt x))
    log (x:+:y)               = log x :+: (y / x)
    sin (x:+:y)               = sin x :+: (y * cos x)
    cos (x:+:y)               = cos x :+: (- y * sin x)
    tan (x:+:y)               = tan x :+: (1 / cos x ** 2)
    asin (x:+:y)              = asin x :+: (y / sqrt(1 - x ** 2))
    acos (x:+:y)              = acos x :+: (- y / sqrt(1 - x ** 2))
    atan (x:+:y)              = atan x :+: (y / (1 + x ** 2))
    sinh (x:+:y)              = sinh x :+: (y * cosh x)
    cosh (x:+:y)              = cosh x :+: (y * sinh x)
    tanh (x:+:y)              = tanh x :+: (y * (1 - tanh x ** 2))
    asinh (x:+:y)             = asinh x :+: (y / sqrt(1 + x ** 2))
    acosh (x:+:y)             = acosh x :+: (y / sqrt(x ** 2 - 1))
    atanh (x:+:y)             = atanh x :+: (y / (1 - x ** 2))
    (x:+:y) ** (x':+:y')      = (x ** x') 
                                  :+: (x ** x' * (y' * log x + (x' * y / x)))
    logBase (x:+:y) (x':+:y') = logBase x x' :+: (k / d)
                                where k = log x' * y / x - log x * y' / x'
                                      d = log x ** 2