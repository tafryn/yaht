-- DON'T forget the syntactically significant whitespace
-- Make sure to use only spaces. Spaces and tabs = trouble
-- Prelude> 5*4+3
-- Prelude> 5^5-2
-- Prelude> sqrt 2                  -- note that parens are not required around funciton arguments
-- Prelude> 5*(4-2)
-- Prelude> (5,3)                   -- This is a pair. n-tuples are possible for an arbitrary n.
                                    -- Pairs can be mixed-type. Lists must be homogeneous.
-- Prelude> fst (3, "hello")
-- Prelude> snd (3, "hello")
-- Prelude> [1,2]                   -- This is a list
-- Prelude> 0:[1,2]                 -- The ':' is the cons operator. It constructs lists.
-- Prelude> 1:2:3:4:[]              -- It can be used several times in sequence.
-- Prelude> [(1,3),(2,4)]
-- Prelude> ([1,3,2],[4,6,5])
-- Prelude> head [1,2,3]            -- Returns one element.
-- Prelude> tail [1,2,3]            -- Returns a list.
-- Prelude> length [1,2,3]          -- Returns the length of a list.
-- Prelude> 'H':'e':'l':'l':'o':[]  -- Strings are lists of characters.
-- Prelude> "Hello, " ++ "World"    -- The '++' is the list concatenation operator.
-- Prelude> "Result:" ++ show (5*5) -- 'show' converts non-strings into strings 
-- Prelude> read "5" + 3            -- 'read' converts strings into non-strings
--
-- There are three main list processing functions: map, filter and foldr
--
-- Prelude> map Char.toUpper "Hello, World"
-- Prelude> filter Char.isLower "Hello, World"
-- Prelude> foldr (+) 0 [3,8,12,5]  -- Sums the list
-- foldr is right associative and foldl is left associative

module Main
    where
    
    import Char
    import IO

    x = 5
    y = (6, "Hello")
    z = x * fst y
    makeList = 1 : makeList

    square x = x * x

    signum x = 
        if x < 0
            then -1
        else if x > 0
            then 1
            else 0
    f x =
        case x of
        0 -> 1
        1 -> 5
        2 -> 2
        _ -> -1
    g 0 = 1
    g 1 = 5
    g 2 = 2
    g _ = -1                        -- Order matters with piecewise definintions.

    quadratic a b c =
        let det = sqrt (b*b - 4*a*c)
        in ((-b + det) / (2*a),
            (-b - det) / (2*a))

    a1 = 5 + 10
    a2 = (+) 5 10
    b1 = map Char.toUpper "Hello World"
    b2 = Char.toUpper `map` "Hello World"

    -- Comment
    {- Comment -}

    factorial 1 = 1
    factorial n = n * factorial (n-1)
    
    my_exponent a 1 = a
    my_exponent a b = a * my_exponent a (b-1)

    my_length [] = 0
    my_length (x:xs) = 1 + my_length xs

    my_filter p [] = []
    my_filter p (x:xs) =
        if p x 
        then x : my_filter p xs
        else my_filter p xs 

    fib 1 = 1
    fib 2 = 1
    fib x = fib(x-1) + fib(x-2)

    -- Positive Integer Multiplication
    mult a 1 = a
    mult a b = a + mult a (b-1)

    my_map f [] = []
    my_map f (x:xs) = f(x) : my_map f xs

{-
    INCORRECTaskForWords = do
        putStrLn "Please enter a word:"
        word <- getLine
        if word == ""
            then return []
            else return (word : INCORRECTaskForWords)
-}
    -- 'do' is used to sequence actions. It is not necessary if there is 
    -- only one action.
    askForWords = do
        putStrLn "Please enter a word:"
        word <- getLine
        if word == ""
            then return []
            else do
                rest <- askForWords
                return (word : rest)

    askForNums = do
        putStrLn "Please enter a number:"
        num <- getLine
        let n = read num
        if n > 0
            then do 
            rest <- askForNums
            return ((n::Int) : rest)
            else if n < 0
                then do
                rest <- askForNums
                return ((n::Int) : rest)
                else 
                    return []

    main1 = do
        nums <- askForNums
        {- Since function calling has such high precidence the parens are 
        necessary around the arguments to putStrLn. Otherwise, the ++ operator
        will try to join 'putStrLn "Sum:"' which is IO() typed and 'show(sum nums)'
        which is [a] typed -}
        putStrLn ("Sum:" ++ show (sum nums))
        putStrLn ("Product:" ++ show(foldr (*) 1 nums))
        putStrLn ("Factorials:" ++ show(map factorial nums))

    lsquare = \x -> x*x
    lf = \x y -> 2*x + y
    
    -- Prelude> :t (+)
    -- Prelude> :t (*)
    -- Prelude> :t (++)
    -- Prelude> :t (:)

    -- The following shows how to using IOStrings with a function, f, that
    -- takes strings. The <- is what actually does this conversion and the 'do'
    -- is necessary to use the <- ?
    main2 = do
        s <- readFile "someFile"
        let i = 'a':s 
        putStrLn (show i)

    tdsquare :: Num a => a -> a
    tdsquare x = x*x

    -- More refined, limited definition of square
    refined_square :: Int -> Int
    refined_square x = x*x

{-    overload :: Int -> Int-}
    {-overload x = x-}
    {-overload :: Double -> Double-}
    {-overload x = 2*x-}

    -- Example using the 'Maybe' type
    firstElement :: [a] -> Maybe a
    firstElement []     = Nothing
    firstElement (x:xs) = Just x

    -- Detour into data.hs

    stackDepth :: Int -> IO()
    stackDepth x = do
        if (x `mod` 1000000) == 0
            then do putStrLn (show x)
            else putStr ""
        stackDepth(x+1)

    mainHello = do
        hSetBuffering stdin LineBuffering
        putStrLn "Please enter your name: "
        name <- getLine
        putStrLn ("Hello, " ++ name ++ ", how are you?")

    -- Detour into Guess.hs

    -- Detour into Data.hs

    -- Detour into iotest.hs

    gtTup :: Num a => Ord a => [(a,b)] -> [(a,b)]
    gtTup lst = filter (\ (a,b) -> a > 0) lst

    etaGtTup0 = filter (\ (a,b) -> a > 0)
    etaGtTup1 = filter (\x -> fst x > 0)
    etaGtTup2 = filter (\x -> ((>0) . fst) x)
    etaGtTup3 = filter ((>0) . fst)

    func1 x l = map (\y -> y*x) l
    func1' x = map (*x)

    func2 f g l = filter f (map g l)
    func2' f g = filter f . map g 

    pair x = (x,x)
    func3 f l = l ++ map f l
    --impossible
    --func3' f = uncurry (++) . pair 

    func4 l = map (\y -> y+2) (filter (\z -> z `elem` [1..10]) (5:l))
    func4' = map (+2) . filter (`elem` [1..10]) . ((:) 5)

    func5 f l = foldr (\x y -> f (y,x)) 0 l
    func5' f = foldr (flip . curry $ f) 0

    -- Pattern Matching

    data Color
        = Red
        | Orange
        | Yellow
        | Green
        | Blue
        | Purple
        | White
        | Black
        | Custom Int Int Int -- R G B components
        deriving (Show,Eq)

    colorToRGB Red = (255,0,0)
    colorToRGB Orange = (255,128,0)
    colorToRGB Yellow = (255,255,0)
    colorToRGB Green = (0,255,0)
    colorToRGB Blue = (0,0,255)
    colorToRGB Purple = (255,0,255)
    colorToRGB White = (255,255,255)
    colorToRGB Black = (0,0,0)
    colorToRGB (Custom r g b) = (r,g,b)

    isBright = isBright' . colorToRGB
        where isBright' (255,_,_) = True
              isBright' (_,255,_) = True
              isBright' (_,_,255) = True
              isBright' _         = False

    rgbToColor 255   0   0 = Just Red
    rgbToColor 255 128   0 = Just Orange
    rgbToColor 255 255   0 = Just Yellow
    rgbToColor   0 255   0 = Just Green
    rgbToColor   0   0 255 = Just Blue
    rgbToColor 255   0 255 = Just Purple
    rgbToColor 255 255 255 = Just White
    rgbToColor   0   0   0 = Just Black
    rgbToColor   r   g   b = 
        if 0 <= r && r <= 255 &&
           0 <= g && g <= 255 &&
           0 <= b && b <= 255
            then Just (Custom r g b)
            else Nothing -- invalid RGB value

    rgbIsValid r g b = rgbIsValid' (rgbToColor r g b)
        where rgbIsValid' (Just _) = True
              rbbIsValid' _        = False

    -- Guards
    comparison x y | x < y = "The first is less"
                   | x > y = "The second is less"
                   | otherwise = "They are equal"

    -- This will NOT fall through as expected.
    comparison2 x y | x < y = "The first is less"
                    | x > y = "The second is less"
    comparison2 _ _ = "They are equal"

    -- Instance Declarations

    {-instance Eq Color where
        Red == Red = True
        Orange == Orange = True
        Yellow == Yellow = True
        Green == Green = True
        Blue == Blue = True
        Purple == Purple = True
        White == White = True
        Black == Black = True
        (Custom r g b) == (Custom r' g' b') =
            r == r' && g == g' && b == b'
        _ == _ = False-}

    stdMap _ [] = []
    stdMap f (x:xs) = f x : map f xs

    stdFilter _ [] = []
    stdFilter p (x:xs) | p x = x : filter p xs
                       | otherwise = filter p xs

    -- Write `and` in terms of foldr
    stdAnd [] = True
    stdAnd (x:xs) = if x then stdAnd xs else False



    -- Write concatMap f in terms of foldr
    main = do
        putStrLn("TEST")

-- vim: ts=4 sw=4 et:
