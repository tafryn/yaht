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
firstTwo (Quadruple a b c d) = [a, b]

lastTwo :: (Quadruple a b) -> [b]
lastTwo (Quadruple a b c d) = [c, d]

data MyTuple a b c d = One a
                     | Two a b
                     | Three a b c
                     | Four a b c d

tuple1 :: (MyTuple a b c d) -> Maybe a
tuple1 (One a       ) = Just a
tuple1 (Two a b     ) = Just a
tuple1 (Three a b c ) = Just a
tuple1 (Four a b c d) = Just a

tuple2 :: (MyTuple a b c d) -> Maybe b
tuple2 (One a       ) = Nothing
tuple2 (Two a b     ) = Just b
tuple2 (Three a b c ) = Just b
tuple2 (Four a b c d) = Just b

tuple3 :: (MyTuple a b c d) -> Maybe c
tuple3 (One a       ) = Nothing
tuple3 (Two a b     ) = Nothing
tuple3 (Three a b c ) = Just c
tuple3 (Four a b c d) = Just c

tuple4 :: (MyTuple a b c d) -> Maybe d
tuple4 (One a       ) = Nothing
tuple4 (Two a b     ) = Nothing
tuple4 (Three a b c ) = Nothing
tuple4 (Four a b c d) = Just d

tupleTranslate :: (MyTuple a b c d) -> Either a (Either (a,b) (Either (a,b,c) (a,b,c,d)))
tupleTranslate (One a       ) = Left a
tupleTranslate (Two a b     ) = Right (Left (a,b))
tupleTranslate (Three a b c ) = Right (Right (Left (a,b,c)))
tupleTranslate (Four a b c d) = Right (Right (Right (a,b,c,d)))


data List a = Nil
            | Cons a (List a)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead (Cons x xs) = x

listTail (Cons x xs) = xs

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr f i Nil = i
listFoldr f i (Cons x xs) = listFoldr f (f x i) xs

listFoldl :: (a -> b -> a) -> a -> List b -> a
listFoldl f i Nil = i
listFoldl f i (Cons x xs) = listFoldl f (f i x) xs

data BinaryTree a = Leaf a
                  | Branch (BinaryTree a) a (BinaryTree a)

treeSize (Leaf x) = 1
treeSize (Branch left x right) = 1 + treeSize(left) + treeSize(right)

btElements (Leaf x) = [x]
btElements (Branch left x right) = btElements(left) ++ [x] ++ btElements(right)

treeFold :: (a -> b -> b) -> b -> (BinaryTree a) -> b
treeFold f i (Leaf x) = f x i
treeFold f i (Branch left x right) = treeFold f (f x (treeFold f i left)) right 

btElements2 x = treeFold (\y z -> z ++ [y]) [] x

data Color
    = Red
    | Green
    | Blue

colorToRGB Red   = (255,0,0)
colorToRGB Green = (0,255,0)
colorToRGB Blue  = (0,0,255)

cfold' f z [] = z
cfold' f z (x:xs) = f x z (\y -> cfold' f y xs)

cfold f z l = cfold' (\x t g -> f x (g t)) z l

cmap' f [] = []
cmap' f (x:xs) = f x (cmap' f xs)

cmap f lst = cmap' (\x y -> (f x) : y) lst

cfilter' f [] = []
cfilter' f (x:xs) = f x (cfilter' f xs)

cfilter f lst = cfilter' (\x y -> if (f x) then x : y else y) lst

-- vim: ts=4 sw=4 et:
