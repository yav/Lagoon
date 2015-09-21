
data SP i o = Get (i -> SP i o)
            | Put o (SP i o)

(>>>) :: SP a b -> SP b c -> SP a c
x        >>> Put o k  = Put o (x >>> k)
Get k1   >>> sp2      = Get (\i -> k1 i >>> sp2)
Put o k1 >>> Get k2   = k1 >>> k2 o


