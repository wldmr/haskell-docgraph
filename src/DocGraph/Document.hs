module DocGraph.Document where

import Data.List
import Data.Tree

import DocGraph.Types
import qualified DocGraph.Types as DT

import Text.Pandoc
import Text.Pandoc.Definition
import qualified Text.Pandoc.Definition as PD
import Text.Pandoc.Shared
import Text.Pandoc.Walk

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
    where (subs, blocks) = partition isSec subElems
          item = createItem (stringify heading) blocks
          -- Helper
          isSec :: Element -> Bool
          isSec (Sec _ _ _ _ _) = True
          isSec _               = False
traverseElem _ = error "FUCK!"


createItem :: String -> [Element] -> Item
createItem heading blocks = Item { itemLabel = heading,
                                   keys = [heading],
                                   itemStyle = "",
                                   links = concatMap findLinks blocks }
    where findLinks (Blk b) = query fnd b
          findLinks _ = error "FUCK!!!"
          fnd (PD.Link label (target, title)) = [DT.Link DT.Out theTarget "" title]
              where theTarget = case target of
                                     "" -> stringify label
                                     t  -> t
          fnd _ = []
