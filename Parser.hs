module Parser(parse,expr,input,Input(..)) where

import Data
import Prelude hiding (abs)

import Control.Applicative
import Data.Char
import Data.Maybe
import Text.ParserCombinators.ReadP as P

-- parse a string completely
parse :: ReadP a -> String -> Maybe a
parse p = fmap fst . listToMaybe . filter (null . snd) . readP_to_S p

{- Parse lambda calculus expressions -}

anon :: ReadP Anon
anon = fmap Anon $ satisfy isAsciiLower >>= (<$> extension) . (:)
  where
    extension = munch $ flip any [(== '\''),isDigit] . flip ($)

name :: ReadP Name
name = fmap Name $ munch1 isAsciiUpper

instance Read E where
  readsPrec _ = readP_to_S expr

expr :: ReadP E
expr = choice [var,abs,app,nam,par]

var :: ReadP E
var = Var <$> anon

abs :: ReadP E
abs = char '\\' *> skipSpaces *> anon <* skipSpaces >>= go
  where
    go x = (anon <* skipSpaces >>= fmap (Abs x) . go) <|>
      (char '.' >> Abs x <$> choice [var,abs,app,nam,par])

app :: ReadP E
app = choice [var,nam,par] <* skipSpaces >>=
  (choice [var,abs,nam,par] <* skipSpaces >>=) . (go .) . App
  where
    go f = (choice [var,abs,nam,par] <* skipSpaces >>= go . App f) <|> return f

nam :: ReadP E
nam = Nam <$> name

par :: ReadP E
par = char '(' *> skipSpaces *> expr <* skipSpaces <* char ')'

{- Parse user input -}

data Input
  = Command String
  | Binding Name E
  | Expr    E
  deriving Show

instance Read Input where
  readsPrec _ = readP_to_S input

input :: ReadP Input
input = choice [command,binding,Expr <$> expr]

command :: ReadP Input
command = fmap Command $ char ':' *> P.many get <* eof

binding :: ReadP Input
binding = name <* skipSpaces <* char '=' <* skipSpaces >>= (<$> expr) . Binding
