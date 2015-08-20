module DocGraph.IO where

import Control.Monad
import Data.Tree
import System.Directory
import System.FilePath

import DocGraph.Types
import DocGraph.Util

traverseDirectory :: FilePath -> IO DocGraph
traverseDirectory path = unfoldTreeM dirContents path

dirContents :: FilePath -> IO (Item, [FilePath])
dirContents path = do subitems <- ifM (doesDirectoryExist path) items noItems
                      return (newItem path, subitems)
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

