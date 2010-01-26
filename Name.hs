module Main
    where

import IO

main = do
    hSetBuffering stdin LineBuffering
    putStrLn "Please enter your name: "
    name <- getLine
    putStrLn ("Hello, " ++ name ++ ", how are you?")


-- vim: ts=4 sw=4 et:
