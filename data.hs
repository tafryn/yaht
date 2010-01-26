data Pair a b = Pair a b

-- Prelude> :t Pair
-- Prelude> :t Pair 'a'
-- Prelude> :t Pair 'a' "Hello"

pairFst :: (Pair x y) -> x
pairFst (Pair x y) = x

pairSnd (Pair x y) = y

data Triple a b c = Triple a b c

tripleFst (Triple a b c) = a
tripleSnd (Triple a b c) = b
tripleThr (Triple a b c) = c

data Quadruple a b = Quadruple a a b b

firstTwo :: (Quadruple a b) -> [a]
firstTwo (Quadruple h j k l) = [h, j]

lastTwo :: (Quadruple a b) -> [b]
lastTwo (Quadruple h j k l) = [k, l]


-- vim: ts=4 sw=4 et:
