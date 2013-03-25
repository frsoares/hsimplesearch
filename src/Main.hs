{-# LANGUAGE CPP, TemplateHaskell, DoAndIfThenElse, PatternGuards #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Francisco Soares
-- License     :  GPL-2
--
-- Maintainer  :  Francisco Soares
-- Stability   :  experimental
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Control.Monad (unless)
import System.Exit
import Test.QuickCheck.All (quickCheckAll)
import Search.Indexer
import Search.Interpreter
import qualified Control.Exception as Exc
import System.IO
import System.CPUTime
import System.Console.GetOpt
import System.Environment (getArgs)
import Text.Printf


-- == Command-line options ==

data Flag = Help | IndexingThreads Int | QueryingThreads Int deriving Eq

parseArgs :: IO ([String], Int, Int)
parseArgs = do 
    argv <- getArgs
    case parse argv of
        ([], files, [])    -> return (files, 3, 3)
        (opts, files, []) 
            | Help `elem` opts   -> help
            | [IndexingThreads n]  <- filter (/=Help) opts -> return (files, n, 3)
            | [QueryingThreads n]  <- filter (/=Help) opts -> return (files, 3, n)
            | [IndexingThreads ni, QueryingThreads nq]  <- filter (/=Help) opts -> return (files, ni, nq)
        (_, _, errs)             -> die errs
      where
        parse argv = getOpt Permute options argv
        header     = "Usage: hsimplesearch [-h] [-i n] [-q n] folder"
        info       = usageInfo header options
        dump       = hPutStrLn stderr
        die errs   = dump (concat errs ++ info) >> exitWith (ExitFailure 1)
        help       = dump info                  >> exitSuccess


options :: [OptDescr Flag]
options =
    [ Option "h" ["help"] (NoArg Help) "Show this help message",
      Option "i" ["indexthread"] (ReqArg (\s -> IndexingThreads (read s)) "Indexing Threads")
          "Number of concurrent threads for indexing (default 3)",
      Option "q" ["querythread"] (ReqArg (\s -> QueryingThreads (read s)) "Querying Threads")
          "Number of concurrent threads for querying (default 3)"]

-- == end of options parsing code ==

exeMain = do
    (folder, indexThreads, queryThreads) <- parseArgs
    let selectedFolder = if null folder then "."
                         else head folder
    putStrLn $ "Indexing folder "++show selectedFolder++"..."
    indexed <- indexFile selectedFolder

    findFile indexed


findFile indexed = do
        putStr "query> "
        hFlush stdout
        query <- getLine
        unless (query `elem` [":q", ":quit"]) $ do 
            start <- getCPUTime
            result <- doInterpret query indexed
            end   <- getCPUTime

            let diff = (end - start) `div` (10^3)

            let queryTime = printf "Time required for query: %d nanoseconds." diff

            mapM_ print $ take 50 result
            putStrLn "====================================================="
            putStrLn $ "Total amount of matching files: "++show (length result)
            putStrLn "====================================================="
            putStrLn $ queryTime 
            putStrLn "====================================================="
            findFile indexed


simpleFileSize path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size


-- Entry point for unit tests.
testMain = do
    allPass <- $quickCheckAll -- Run QuickCheck on all prop_ functions
    unless allPass exitFailure

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main = MAIN_FUNCTION


