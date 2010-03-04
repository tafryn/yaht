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
        putStrLn ("Factorias:" ++ show(map factorial nums))

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
-- vim: ts=4 sw=4 et:
