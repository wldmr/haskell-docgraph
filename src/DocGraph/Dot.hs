{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}
module DocGraph.Dot where

import qualified Data.Tree as DT
import Data.List

type Graph = (DT.Tree Node, [Edge])
type Node = (ID, [Attribute])
type Edge = (ID, ID, [Attribute])
type Attribute = (Name, Value)

type ID = String
type Name = String
type Value = String

instance Show Graph where
    show (DT.Node (i, as) subs, es) = "digraph {\n"
                                 ++ intercalate ";\n" (map show as)
                                 ++ "\n\n"
                                 ++ intercalate "\n" (map show subs)
                                 ++ "\n\n"
                                 ++ intercalate ";\n" (map show es)
                                 ++ "\n}"

instance Show (DT.Tree Node) where
    -- Nodes (no children)
    show (DT.Node (i, []) []) = i++";"
    show (DT.Node (i, as) []) = i ++ " ["++attrs ++"];"
        where attrs = intercalate ", " (map show as)

    -- Clusters
    show (DT.Node (i, as) subs) = "subgraph cluster_"++i++" {\n"
                             ++ intercalate ";\n" (map show as)
                             ++ "\n\n"
                             ++ intercalate "\n" (map show subs)
                             ++ "\n}"

instance Show Edge where
    show (i1, i2, []) = i1++" -> "++i2
    show (i1, i2, as) = i1++" -> "++i2++" ["++attrs++"]"
        where attrs = intercalate ", " (map show as)

instance Show Attribute where
    show (n, v) = n++"=\""++v++"\""
