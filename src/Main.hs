module Main where

import Data.Tree
import System.Environment

import DocGraph

main :: IO ()
main = do
    args <- getArgs
    tree <- traverseAllIO $ if null args
                                    then "test-data"
                                    else head args
    putStr $ drawTree $ fmap show tree
    let dot = graph2dot tree
    writeFile "test.dot" dot
    putStrLn ""
    putStrLn "-----"
    putStrLn ""
    putStrLn dot
