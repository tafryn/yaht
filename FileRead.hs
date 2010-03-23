module Main
    where

import IO

main = do
    hSetBuffering stdin LineBuffering
    doLoop

doLoop = do
    putStrLn "Enter a command rFN or wFN or q to quit:"
    command <- getLine
    case command of
        'q':_ -> return ()
        'r':filename -> do putStrLn ("Reading " ++ filename)
                           doRead filename
                           doLoop
        'w':filename -> do putStrLn ("Writing " ++ filename)
                           doWrite filename
                           doLoop
        _            -> doLoop


doRead filename = 
    bracket (openFile filename ReadMode) hClose
            (\h -> do contents <- hGetContents h
                      putStrLn "The first 100 chars:"
                      putStrLn (take 100 contents))
                      
{-
doRead filename = do
    contents <- readFile filename
    putStrLn "The first 100 chars:"
    putStrLn (take 100 contents)
-}
doWrite filename = do
    putStrLn "Enter text to go into the file:"
    contents <- getLine
    bracket (openFile filename WriteMode) hClose
            (\h -> hPutStrLn h contents)
-- vim: ts=4 sw=4 et: