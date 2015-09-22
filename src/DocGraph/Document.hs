module DocGraph.Document where

import Data.Either (rights)
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

type Parser = Parsec String ()

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

node :: Parser Token
node = do hs <- many1 (char '#')
          skipMany space
          s <- many1 (noneOf "#\n")
          let lvl = length hs
          return $ TNode lvl s

link :: Parser Token
link = do string "->"
          skipMany space
          s <- manyTill anyChar eof
          return $ TLink s

itemize :: [Token] -> [(Int, Item)]
itemize [] = []
itemize (TNode l s : ts) = (l, newItem s) : itemize ts'
    where (_, ts') = break atNode ts
          -- TODO: Do something useful with the first part.
          atNode (TNode {}) = True
          atNode _          = False

build :: [(Int, Item)] -> [DocGraph]
build [] = []
build ((n, itm):rest) = (DT.Node itm (build children)) : build siblings
    where (children, siblings) = span ((>n).fst) rest
