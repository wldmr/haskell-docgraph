module DocGraph.Document where

import Data.Either (rights)
import qualified Data.Tree as DT

import DocGraph.Types

import Text.Parsec

traverseDocumentIO :: FilePath -> IO DocGraph
traverseDocumentIO path = do
    contents <- readFile path
    let forest = map toTree $ tokenize contents
    return $ DT.Node (newItem path) forest
  where
    toTree t = DT.Node (newItem $ show t) []

data Token = TNode Int String

instance Show Token where
    show (TNode level label) = "("++show level++") " ++ label

tokenize :: String -> [Token]
tokenize s = rights $ map (parse' node) (lines s)

type Parser = Parsec String ()

parse' :: Parser a -> String -> Either ParseError a
parse' p = parse p ""

node :: Parser Token
node = do hs <- many1 (char '#')
          skipMany space
          s <- many1 (noneOf "#\n")
          let lvl = length hs
          return $ TNode lvl s
