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
traverseDocument (Pandoc meta blocks) = unfoldTree traverseElem topElem
    where
        elements = hierarchicalize blocks
        topElem = Sec 0 [] nullAttr (docTitle meta) elements

traverseElem :: Element -> (Item, [Element])
-- Reminder: Sec level nums attr heading subElems
traverseElem (Sec _ _ _ heading subElems) = (item, subs)
    where item = newItem (stringify heading)
          subs = filter validElem subElems
traverseElem _ = error "FUCK!"

validElem :: Element -> Bool
validElem (Sec _ _ _ _ _) = True
validElem _               = False
