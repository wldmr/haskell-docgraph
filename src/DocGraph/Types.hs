module DocGraph.Types where

import Data.Tree

newtype Item = Item { itemLabel :: String }

instance Show Item where
    show = itemLabel


type DocGraph = Tree Item
