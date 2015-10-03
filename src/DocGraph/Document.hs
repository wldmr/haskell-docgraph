module DocGraph.Document where

import Control.Arrow ((>>>))
import Data.Char (isSpace)
import Data.Either (rights)
import Data.List (dropWhile, dropWhileEnd)
import qualified Data.Tree as DT

import DocGraph.Types

import Text.Parsec hiding (token)

traverseDocumentIO :: FilePath -> IO DocGraph
traverseDocumentIO path = do
    contents <- readFile path
    return $ DT.Node (newItem path) (build $ itemize $ tokenize contents)

data Token = TNode Int String
           | TLink String
           deriving (Show)

tokenize :: String -> [Token]
tokenize s = rights $ map (parse' tok) (lines s)
    where tok = choice [node, link]

strip :: Label -> Label
strip = dropWhile isSpace >>> dropWhileEnd isSpace

type Parser = Parsec String ()

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

node :: Parser Token
node = do hs <- many1 (char '#')
          skipMany space
          s <- many1 (noneOf "#\n")
          let lvl = length hs
          return $ TNode lvl (strip s)

link :: Parser Token
link = do string "->"
          skipMany space
          s <- manyTill anyChar eof
          return $ TLink (strip s)

itemize :: [Token] -> [(Int, Item)]
itemize [] = []
itemize (TNode l s : ts) = (l, itm) : itemize ts'
    where (subs, ts') = break atNode ts

          itm = newItem s `linkToAll` links
          links = map asStr $ filter isLink subs

          atNode (TNode {}) = True
          atNode _          = False

          isLink (TLink {}) = True
          isLink _          = False

          asStr (TLink s') = s'
          asStr _ = error "asStr should only be called on TLinks"
itemize (TLink {}:_) = error "TLink encountered. Should have been filtered out."


build :: [(Int, Item)] -> [DocGraph]
build [] = []
build ((n, itm):rest) = (DT.Node itm (build children)) : build siblings
    where (children, siblings) = span ((>n).fst) rest
