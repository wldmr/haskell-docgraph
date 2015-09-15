module Main where

import Data.Tree
import System.Environment

import DocGraph
import DocGraph.Document (traverseDocumentIO)

main :: IO ()
main = do
    args <- getArgs
    tree <- traverseAllIO $ if null args
                                    then "test-data"
                                    else head args
    putStr $ drawTree $ fmap show tree
