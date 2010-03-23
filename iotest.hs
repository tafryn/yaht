module Main
    where

    import IO

    fileGrab = do
        fh <- openFile "foo" ReadMode
        contents <- hGetContents fh
        putStr(contents)
        hClose fh
    
    doRead = do
        putStrLn "File to read:"
        file <- getLine
        putStrLn ("-----Contents of " ++ file ++ "-----")
        contents <- readFile file
        putStr contents
    
    doWrite = do
        putStrLn "File to write:"
        file <- getLine
        putStrLn "File contents"
        contents <- gatherLoop
        writeFile file contents

    gatherLoop = do
        line <- getLine
        case line of
            "." -> return []
            _   -> do rest <- gatherLoop
                      return (line ++ rest)

    exercise = do
        putStrLn "Do you want to [read], [write], or [quit]?"
        verb <- getLine
        case verb of
            "quit" -> return ()
            "read" ->  doRead
            "write" -> doWrite
            _       -> exercise

    main = do
        exercise
-- vim: ts=4 sw=4 et:
