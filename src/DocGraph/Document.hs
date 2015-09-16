module DocGraph.Document where

import Data.Maybe (catMaybes)
import Data.Tree

import DocGraph.Types

import Text.Pandoc
import Text.Pandoc.Shared hiding (err)

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
        -- Reminder: Sec level nums attr heading subElems

traverseElem :: Element -> (Item, [Element])
traverseElem (Sec _ _ _ heading subElems) = (item, subs)
    where item = newItem (stringify heading) `linkToAll` links
          subs = filter isSec subElems
          links = catMaybes (map maybeLink subElems)
          -- Helper
          isSec (Sec _ _ _ _ _) = True
          isSec _               = False

          maybeLink (Blk (Para [Str s])) = Just s
          maybeLink _                    = Nothing
traverseElem _ = error "FUCK!"
