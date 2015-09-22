module DocGraph where

import Data.Tree
import System.Directory

import DocGraph.Document
import DocGraph.Directory
import DocGraph.Types

traverseAllIO :: FilePath -> IO DocGraph
traverseAllIO path = do p <- getPathType path
                        unfoldTreeM contents p

data BuildingBlock = Directory FilePath
                   | MarkdownFile FilePath
                   | DGraph DocGraph

getPathType :: FilePath -> IO BuildingBlock
getPathType path = do
    isDir <- doesDirectoryExist path
    return $ (if isDir then Directory else MarkdownFile) path

contents :: BuildingBlock -> IO (Item, [BuildingBlock])
contents (Directory path) = do (itm, paths) <- dirContents path
                               blocks <- sequence $ map getPathType paths
                               return (itm, blocks)
contents (MarkdownFile path) = do doc <- traverseDocumentIO path
                                  contents (DGraph doc)
contents (DGraph dg) = return (rootLabel dg, map DGraph $ subForest dg)
