#!/usr/bin/runhaskell
module Main where

import IO
import System(getArgs)

import Lang
import Parser

showResponse :: ProgramResponse -> IO ()
showResponse []     = putStr "done.\n"
showResponse (r:rs) = do
    putStr $ show r
    putStr "\n"
    showResponse rs

checkString :: String -> IO ()
checkString x = do
    case checkProgram =<< parse x of
      OK response -> showResponse response
      Error msg   -> putStr ("Error:\n" ++ msg ++ "\n")

checkFile :: String -> IO ()
checkFile filename = do
    f <- openFile filename ReadMode
    s <- hGetContents f
    checkString s
    hClose f

usage :: String
usage = "Usage: runhaskell Main.hs file.8f.\n" ++
        "\n" ++
        "Compiling with GHC:\n" ++
        "    ghc --make Main.hs -o eightfold\n" ++
        "    ./eightfold file.8f\n"

main :: IO ()
main = do
    argv <- getArgs
    if length argv /= 1
     then putStr usage
     else checkFile (head argv)

