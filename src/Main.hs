module Main where

import Data.Tree
import System.Environment

import DocGraph.Document (traverseDocumentIO)

main :: IO ()
main = do
    args <- getArgs
    tree <- traverseDocumentIO $ if null args
                                    then "test-data/test.md"
                                    else head args
    putStr $ drawTree $ fmap show tree
