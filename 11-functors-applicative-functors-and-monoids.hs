import Control.Applicative
import Data.Monoid

-- We can use the following type to simulate our own list
data List a = Empty | Value a (List a) deriving (Show)

-- Make the list a Functor
instance Functor List where
    fmap f Empty = Empty
    fmap f (Value a list) = Value (f a) (fmap f list)

-- Write a function which appends one list on to another
combineLists:: List a -> List a -> List a
combineLists Empty a = a
combineLists (Value a list) b = Value a $ combineLists list b

-- Make our list a Monoid
instance Monoid (List a) where
    mempty  = Empty
    mappend = combineLists

-- Make our list an Applicative
instance Applicative List where
    pure a = Value a Empty
    Empty      <*> _  = Empty
    Value f fx <*> xs = combineLists (fmap f xs) (fx <*> xs)

-- Make our list a Monad
instance Monad List where
    return a = Value a Empty
    Empty        >>= f = Empty
    Value a list >>= f = combineLists (f a) (list >>= f)

-- Make sure that the List obeys the laws for Applicative and Monoid
{--
= ファンクター則 =
== 第一法則==
[fmap id = id]
*Main> fmap id Empty
Empty
*Main> fmap id (Value 1 $ Value 2 $ Value 3 Empty)
Value 1 (Value 2 (Value 3 Empty))

== 第二法則 ==
[fmap (f . g) = fmap f . fmap g]
*Main> fmap (show . (+1)) Empty
Empty
*Main> fmap show . fmap (+1) $ Empty
Empty
*Main> fmap show (fmap (+1) Empty)
Empty

*Main> fmap (show . (+1)) (Value 1 $ Value 2 $ Value 3 Empty)
Value "2" (Value "3" (Value "4" Empty))
*Main> fmap show . fmap (+1) $ (Value 1 $ Value 2 $ Value 3 Empty)
Value "2" (Value "3" (Value "4" Empty))
*Main> fmap show (fmap (+1) (Value 1 $ Value 2 $ Value 3 Empty))
Value "2" (Value "3" (Value "4" Empty))

= モノイド則 =
[mempty `mappend` x = x]
*Main> mempty `mappend` Empty
Empty
*Main> mempty `mappend` (Value 1 Empty)
Value 1 Empty

[x `mappend` mempty = x]
*Main> Empty `mappend` mempty
Empty
*Main> (Value 1 Empty) `mappend` mempty
Value 1 Empty

[(x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)]
*Main> (Empty `mappend` Empty) `mappend` Empty
Empty
*Main> Empty `mappend` (Empty `mappend` Empty)
Empty

*Main> (Value 1 Empty `mappend` Value 2 Empty) `mappend` Value 3 Empty
Value 1 (Value 2 (Value 3 Empty))
*Main> Value 1 Empty `mappend` (Value 2 Empty `mappend` Value 3 Empty)
Value 1 (Value 2 (Value 3 Empty))

*Main> (Empty `mappend` Value 2 Empty) `mappend` Value 3 Empty
Value 2 (Value 3 Empty)
*Main> Empty `mappend` (Value 2 Empty `mappend` Value 3 Empty)
Value 2 (Value 3 Empty)

= アプリカティブ則 =
[pure f <*> x = fmap f x]
*Main> pure (+1) <*> Empty
Empty
*Main> fmap (+1) Empty
Empty

*Main> pure (+1) <*> (Value 1 $ Value 2 Empty)
Value 2 (Value 3 Empty)
*Main> fmap (+1) (Value 1 $ Value 2 Empty)
Value 2 (Value 3 Empty)

[pure id <*> v = v]
*Main> pure id <*> Empty
Empty

*Main> pure id <*> (Value 1 $ Value 2 $ Value 3 Empty)
Value 1 (Value 2 (Value 3 Empty))

[pure (.) <*> u <*> v <*> w = u <*> (v <*> w)]
*Main> pure (.) <*> Empty <*> (Value (+2) $ Value (*3) Empty) <*> (Value 1 $ Value 2 Empty)
Empty
*Main> pure (.) <*> (Value (+1) $ Value (*2) Empty) <*> Empty <*> (Value 1 $ Value 2 Empty)
Empty
*Main> pure (.) <*> (Value (+1) $ Value (*2) Empty) <*> (Value (+2) $ Value (*3) Empty) <*> Empty
Empty

*Main> Empty <*> ((Value (+2) $ Value (*3) Empty) <*> (Value 1 $ Value 2 Empty))
Empty
*Main> (Value (+1) $ Value (*2) Empty) <*> (Empty <*> (Value 1 $ Value 2 Empty))
Empty
*Main> (Value (+1) $ Value (*2) Empty) <*> ((Value (+2) $ Value (*3) Empty) <*> Empty)
Empty

*Main> pure (.) <*> Empty <*> Empty <*> (Value 1 $ Value 2 Empty)
Empty
*Main> pure (.) <*> (Value (+1) $ Value (*2) Empty) <*> Empty <*> Empty
Empty
*Main> pure (.) <*> Empty <*> (Value (+2) $ Value (*3) Empty) <*> Empty
Empty

*Main> Empty <*> (Empty <*> (Value 1 $ Value 2 Empty))
Empty
*Main> (Value (+1) $ Value (*2) Empty) <*> (Empty <*> Empty)
Empty
*Main> Empty <*> ((Value (+2) $ Value (*3) Empty) <*> Empty)
Empty

*Main> pure (.) <*> Empty <*> Empty <*> Empty
Empty

*Main> Empty <*> (Empty <*> Empty)
Empty

*Main> pure (.) <*> (Value (+1) $ Value (*2) Empty) <*> (Value (+2) $ Value (*3) Empty) <*> (Value 1 $ Value 2 Empty)
Value 4 (Value 5 (Value 4 (Value 7 (Value 6 (Value 8 (Value 6 (Value 12 Empty)))))))

*Main> (Value (+1) $ Value (*2) Empty) <*> ((Value (+2) $ Value (*3) Empty) <*> (Value 1 $ Value 2 Empty))
Value 4 (Value 5 (Value 4 (Value 7 (Value 6 (Value 8 (Value 6 (Value 12 Empty)))))))

[pure f <*> pure x = pure (f x)]
*Main> pure (+1) <*> pure 1 :: List Int
Value 2 Empty
*Main> pure ((+1) 1) :: List Int
Value 2 Empty

[u <*> pure y = pure ($ y) <*> u]
*Main> Empty <*> pure 1
Empty
*Main> pure ($ 1) <*> Empty
Empty

*Main> (Value (+1) $ Value (+2) Empty) <*> pure 1
Value 2 (Value 3 Empty)
*Main> pure ($ 1) <*> (Value (+1) $ Value (+2) Empty)
Value 2 (Value 3 Empty)

= モナド則 =
[左恒等性]
*Main> return 1 >>= (\x -> Value (x + 1) Empty)
Value 2 Empty

*Main> (\x -> Value (x + 1) Empty) 1
Value 2 Empty

[右恒等性]
*Main> Empty >>= return
Empty

*Main> Value 1 $ Value 2 $ Value 3 Empty >>= return
Value 1 (Value 2 (Value 3 Empty))

[結合法則]
*Main> Empty >>= \x -> return (x + 1) >>= \y -> return (y * (-1))
Empty

*Main> (Value 1 $ Value 2 $ Value 3 Empty) >>= \x -> return (x + 1) >>= \y -> return (y * (-1))
Value (-2) (Value (-3) (Value (-4) Empty))

*Main> Empty >>= (\a -> (\x -> return (x + 1)) a >>= \y -> return (y * (-1)))
Empty

*Main> (Value 1 $ Value 2 $ Value 3 Empty) >>= (\a -> (\x -> return (x + 1)) a >>= \y -> return (y * (-1)))
Value (-2) (Value (-3) (Value (-4) Empty))

--}

-- Create some lists of numbers of different lengths such as:
twoValueList = Value 10 $ Value 20 Empty

-- Use <$> on the lists with a single-parameter function, such as:
plusTwo = (+2)

{--
*Main> plusTwo <$> twoValueList
Value 12 (Value 22 Empty)
--}

-- Use <$> and <*> on the lists with a binary function
{--
*Main> (+) <$> (Value 1 $ Value 2 $ Value 3 Empty) <*> (Value 4 $ Value 5 Empty)
Value 5 (Value 6 (Value 6 (Value 7 (Value 7 (Value 8 Empty)))))
--}

-- Create some lists of binary functions
binFunctions = Value (+) $ Value (*) $ Empty

-- Use <*> on the binary functions list and the number lists
{--
*Main> binFunctions <*> Empty <*> (Value 4 $ Value 5 Empty)
Empty
*Main> binFunctions <*> (Value 1 $ Value 2 $ Value 3 Empty) <*> Empty
Empty
*Main> binFunctions <*> Empty <*> Empty
Empty

*Main> binFunctions <*> (Value 1 $ Value 2 $ Value 3 Empty) <*> (Value 4 $ Value 5 Empty)
Value 5 (Value 6 (Value 6 (Value 7 (Value 7 (Value 8 (Value 4 (Value 5 (Value 8 (Value 10 (Value 12 (Value 15 Empty)))))))))))
--}
