#!/usr/bin/runhaskell
import IO
import System(getArgs)

import Lang
import Parser

checkString :: String -> IO ()
checkString x = do
    case checkProgram =<< parse x of
      OK prog   -> putStr "done."
      Error msg -> putStr ("Error:\n" ++ msg)

checkFile :: String -> IO ()
checkFile filename = do
    f <- openFile filename ReadMode
    s <- hGetContents f
    checkString s
    hClose f

usage :: String
usage = "Usage: runhaskell Main.hs file.8f\n"

main :: IO ()
main = do
    argv <- getArgs
    if length argv /= 1
     then putStr usage
     else checkFile (head argv)

