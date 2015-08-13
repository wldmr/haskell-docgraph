module Main where

import System.Environment

import DocGraph

main :: IO ()
main = do
    args <- getArgs
    tree <- traverseDirectory $ if null args
                                then "test-data"
                                else head args
    putStr $ drawTree $ fmap show tree
