module Main where

import Control.Monad
import Data.Tree
import System.Directory
import System.Environment
import System.FilePath

data Doc = Item String

type DocGraph = Tree Doc

instance Show Doc where
    show (Item s) = s

-- * Directory Traversal

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

-- * Helper functions

-- | Monadic If
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM predicate ifPart elsePart = do
    isTrue <- predicate
    if isTrue then ifPart else elsePart

-- * Main function
main :: IO ()
main = do
    args <- getArgs
    tree <- traverseDirectory $ if null args
                                then "test-data"
                                else head args
    putStr $ drawTree $ fmap show tree
