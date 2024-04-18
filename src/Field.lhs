> {-# LANGUAGE TypeSynonymInstances #-}
> {-# LANGUAGE FlexibleInstances #-}
> module Field where
> import Data.Ratio
> 
> class Field a where
>   zeroElement :: a
>   identityElement :: a
>   equal :: a -> a -> Bool
>   plus :: a -> a -> a
>   times :: a -> a -> a
>   -- You have to define those 5
>   isZero :: a -> Bool
>   isZero x = x `equal` zeroElement
>   isIdentity :: a -> Bool
>   isIdentity x = x `equal` identityElement
>   -- Define negate or minus (or both)
>   negate :: a -> a
>   negate x = zeroElement `minus` x
>   minus :: a -> a -> a
>   minus x y = x `plus` (Field.negate y)
>   -- Define invert or dividedBy (or both)
>   invert :: a -> a
>   invert x = identityElement `dividedBy` x
>   dividedBy :: a -> a -> a
>   dividedBy x y = x `times` (invert y)
>   -- rendersAsNegative... when rendered, would it have a minus
>   -- sign? i.e. do I need to put a plus sign before it?
>   rendersAsNegative :: a -> Bool
>   rendersAsNegative _ = False
>
> instance (Integral a) => Field (Ratio a) where
>   zeroElement = 0
>   identityElement = 1
>   equal x y = x == y
>   plus x y = x + y
>   times x y = x * y
>   negate x = - x
>   minus x y = x - y
>   invert x = (1%1) / x
>   dividedBy x y = x / y
>   rendersAsNegative x = ((numerator x) < 0) && (denominator x == 1)
> 
