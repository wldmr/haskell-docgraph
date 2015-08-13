module DocGraph ( DocGraph()
                , traverseDirectory
                , drawTree
                ) where

import Control.Monad
import Data.Tree
import System.Directory
import System.FilePath

import DocGraph.Util

data Doc = Item String
type DocGraph = Tree Doc

instance Show Doc where
    show (Item s) = s

traverseDirectory :: FilePath -> IO DocGraph
traverseDirectory path = unfoldTreeM dirContents path

dirContents :: FilePath -> IO (Doc, [FilePath])
dirContents path = do subitems <- ifM (doesDirectoryExist path) items noItems
                      return (Item path, subitems)
    where items = do paths <- getDirectoryContents path
                     let subitems = map (path </>) paths
                     filterM valid subitems
          noItems = return []

valid :: FilePath -> IO Bool
valid path = liftM or $ sequence [isTextFile, isNormalDir]
    where isTextFile = return $ takeExtension path == ".md"
          isNormalDir = do
              isDir <- doesDirectoryExist path
              return $ isDir && head fName /= '.' && last fName /= '.'
          fName = takeFileName path

