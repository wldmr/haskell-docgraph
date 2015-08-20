module Main where

import Data.Tree
import System.Environment

import DocGraph
import DocGraph.IO
import DocGraph.Types

main :: IO ()
main = do
    args <- getArgs
    tree <- traverseDirectory $ if null args
                                then "test-data"
                                else head args
    putStr $ drawTree $ fmap show tree
