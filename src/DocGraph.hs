module DocGraph where

import Control.Arrow ((>>>))
import Control.Monad
import qualified Data.Map.Strict as M
import Data.Tree
import System.Directory
import Text.Dot

import DocGraph.Document
import DocGraph.Directory
import DocGraph.Types

traverseAllIO :: FilePath -> IO DocGraph
traverseAllIO path = do p <- getPathType path
                        unfoldTreeM contents p

data BuildingBlock = Directory FilePath
                   | MarkdownFile FilePath
                   | DGraph DocGraph

getPathType :: FilePath -> IO BuildingBlock
getPathType path = do
    isDir <- doesDirectoryExist path
    return $ (if isDir then Directory else MarkdownFile) path

contents :: BuildingBlock -> IO (Item, [BuildingBlock])
contents (Directory path) = do (itm, paths) <- dirContents path
                               blocks <- sequence $ map getPathType paths
                               return (itm, blocks)
contents (MarkdownFile path) = do doc <- traverseDocumentIO path
                                  contents (DGraph doc)
contents (DGraph dg) = return (rootLabel dg, map DGraph $ subForest dg)

graph2dot :: DocGraph -> String
graph2dot g = showDot $ do attribute ("compound", "true")
                           infos <- dotClusters g
                           let m      = M.fromList (idMapping infos)
                               finder = (m M.!)
                               edges  = mkEdgelist finder infos
                           forM_ edges dotEdge
    where

        -- | Create a tree of dot clusters,
        --   collect info about created nodes along the way.
        dotClusters :: DocGraph -> Dot [NodeInfo]
        dotClusters (Node itm []) = do
            let s = itemLabel itm
            nid <- Text.Dot.node [ ("label", s),
                                   ("shape", "box"),
                                   ("style", "rounded") ]
            return [(itm, NodeAddress nid)]
        dotClusters (Node itm ns) = do
            let s = itemLabel itm
            (cid, (cnid, subNodes)) <- cluster $ do
                attribute ("label", s)
                attribute ("style", "rounded")
                cnid <- Text.Dot.node [ ("shape", "point"),
                                        ("style", "invis"),
                                        ("fixedsize", "true"),
                                        ("height", "0"),
                                        ("width", "0") ]
                subs <- forM ns dotClusters
                return (cnid, concat subs)
            return $ (itm, ClusterAddress cid cnid) : subNodes

        idMapping :: [NodeInfo] -> [(String, NodeInfo)]
        idMapping = map mkAssoc
            where mkAssoc info = (key info, info)
                  key = itemLabel . infoItem

        mkEdgelist :: NodeFinder -> [NodeInfo] -> [(NodeInfo, NodeInfo)]
        mkEdgelist fnd = concatMap mkEdgelist'
            where
                mkEdgelist' info = [ (info, info2) | info2 <- others info ]
                others = map fnd . map linkTarget . itemLinks . infoItem

        dotEdge :: (NodeInfo, NodeInfo) -> Dot ()
        dotEdge (s, t) = edge (address s) (address t) attrs
            where
                address info = case infoAddress info of
                                    NodeAddress      nid -> nid
                                    ClusterAddress _ nid -> nid

                attrs = concat [attach "ltail" s, attach "lhead" t]

                attach attr info = case infoAddress info of
                                     ClusterAddress cid _ -> [(attr, show cid)]
                                     _                    -> []


type NodeFinder = String -> NodeInfo
type NodeMap = M.Map String NodeInfo

type NodeInfo = (Item, Address)
infoItem = fst
infoAddress = snd

data Address = NodeAddress NodeId
             | ClusterAddress ClusterId NodeId
type ClusterId = NodeId  -- Just for clarification
