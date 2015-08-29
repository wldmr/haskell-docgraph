module DocGraph.Document where

import Data.List
import Data.Tree

import DocGraph.Types

import Text.Pandoc
import Text.Pandoc.Definition
import Text.Pandoc.Shared
import Text.Pandoc.Walk
import Text.Pandoc.Writers.Markdown

traverseDocumentIO :: FilePath -> IO DocGraph
traverseDocumentIO path = do
    string <- readFile path
    let result = readMarkdown def string
    case result of
        Left err -> error $ show err
        Right doc -> return $ traverseDocument doc

traverseDocument :: Pandoc -> DocGraph
traverseDocument (Pandoc meta blocks) = Node { rootLabel = newItem title,
                                               subForest = forest }
    where forest = unfoldForest traverseBlock blocks
          title = stringify $ docTitle meta

traverseBlock :: Block -> (Item, [Block])
traverseBlock block = undefined

