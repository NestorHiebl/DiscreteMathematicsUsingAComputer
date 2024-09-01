import Stdm

mux1 :: Signal a => a -> a -> a -> a
mux1 a x y = or2 (and2 (inv a) x) (and2 a y)

demux1 :: Signal a => a -> a -> (a,a)
demux1 a x = (and2 (inv a) x, and2 a x)

bitValue :: Bool -> Integer
bitValue x = if x then 1 else 0

wordValue :: [Bool] -> Integer
wordValue [] = 0
wordValue (x:xs) = (2^i * bitValue x) + wordValue xs
    where i = length xs

