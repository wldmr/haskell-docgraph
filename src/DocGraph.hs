module DocGraph where

import Control.Monad
import Data.Tree
import System.Directory
import Text.Dot

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

graph2dot :: DocGraph -> String
graph2dot g = showDot $ graph2dot' g

graph2dot' :: DocGraph -> Dot ()
graph2dot' (Node (Item s _) []) = do
    Text.Dot.node $ [ ("label", s) ]
    return ()
graph2dot' (Node (Item s _) ns) = do
    cluster $ do
        attribute ("label", s)
        forM_ ns graph2dot'
    return ()
