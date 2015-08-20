module DocGraph.Types where

import Data.Tree

data Item = Item { itemLabel :: Label,
                   keys :: [Key],
                   itemStyle :: Style,
                   links :: [Link] }

data LinkType = In | Out
data Link = Link  { linkType :: LinkType,
                    linkTarget :: Key,
                    linkStyle :: Style,
                    linkLabel :: Label }

type Style = String
type Label = String
type Key   = String

newItem :: String  -> Item
newItem lbl         = Item lbl [lbl] "" []

addKey :: Item -> String -> Item
addKey item key = item { keys = key : keys item }

addLink :: Item -> Link -> Item
addLink item link = item { links = link : links item }

type DocGraph = Tree Item

instance Show Item where
    show = itemLabel
