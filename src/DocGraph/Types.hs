module DocGraph.Types where

import Data.List (intercalate)
import Data.Tree

data Item = Item { itemLabel :: Label,
                   itemLinks :: [Link] }

instance Show Item where
    show itm = itemLabel itm ++ " (→" ++ intercalate "," ls ++ ")"
        where ls = map show (itemLinks itm)

type Label = String

newtype Link = Link { linkTarget :: String }

instance Show Link where
    show (Link s) = s

newItem :: Label -> Item
newItem l = Item l []

linkTo :: Item -> String -> Item
linkTo itm l = itm { itemLinks = Link l : itemLinks itm }

linkToAll :: Item -> [String] -> Item
linkToAll = foldl linkTo

type DocGraph = Tree Item


