module AoCUtils.AoCTuple (
    fst3, snd3, thd3,
    pairUp
) where

-- Triple accessors
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

thd3 :: (a, b, c) -> c
thd3 (_, _, z) = z

-- Pair up elements into tuples, error if odd number
pairUp :: [a] -> [(a, a)]
pairUp [] = []
pairUp [_] = error "pairUp: odd number of elements"
pairUp (x:y:xs) = (x, y) : pairUp xs