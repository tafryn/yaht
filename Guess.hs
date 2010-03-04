module Main
    where

import IO
import Random

main = do
    hSetBuffering stdin LineBuffering
    --num <- randomRIO (1::Int, 100)
    --putStrLn "I'm thinking of a number between 1 and 100"
    --doGuessing num
    doNameGuessIf

doGuessing num = do
    putStrLn "Enter your guess:"
    guess <- getLine
    let guessNum = read guess
    if guessNum < num
        then do putStrLn "Too low!"
                doGuessing num
        else if read guess > num
            then do putStrLn "Too high!"
                    doGuessing num
            else do putStrLn "You Win!"

doNameGuessCase = do
    putStrLn "Enter your name: "
    name <- getLine
    case name of
        "Simon" -> putStrLn "Haskell is great!"
        "John" -> putStrLn "Haskell is great!"
        "Phil" -> putStrLn "Haskell is great!"
        "Koen" -> putStrLn "Debugging haskell is fun!"
        _ -> putStrLn "I don't know you."

doNameGuessIf = do
    putStrLn "Enter your name: "
    name <- getLine
    if name == "Simon"
        then putStrLn "Haskell is great!"
        else if name == "John"
            then putStrLn "Haskell is great!"
            else if name == "Phil"
                then putStrLn "Haskell is great!"
                else if name == "Koen"
                    then putStrLn "Debugging haskell is fun!"
                    else putStrLn "I don't know you."
-- vim: ts=4 sw=4 et:
