module DocGraph.Document where

import Data.Either (rights)
import qualified Data.Tree as DT

import DocGraph.Types

import Text.Parsec hiding (token)

traverseDocumentIO :: FilePath -> IO DocGraph
traverseDocumentIO path = do
    contents <- readFile path
    let forest = map toTree $ tokenize contents
    return $ DT.Node (newItem path) forest
  where
    toTree t = DT.Node (newItem $ show t) []

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
