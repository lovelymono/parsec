module Text.Parsec.String where

import qualified Text.Parsec as T

type Parser = T.Parser String
type Result = T.Result String

just :: Parser Char
just = T.Parser $ \i -> case i of
  [] -> T.Fail i
  c:s -> T.Done s c

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = T.Parser $ \i -> case i of
  [] -> T.Fail i
  c:s -> if p c
    then T.Done s c
    else T.Fail i

char :: Char -> Parser Char
char c = satisfy $ \c' -> c' == c

string :: String -> Parser String
string s = T.Parser $ \i -> case startsWith s i of
  Just t -> T.Done t s
  Nothing -> T.Fail i

-- | Checks whether @t@ starts with a prefix @s@, and returns the suffix. If
-- the strings don't match, returns 'Nothing'.
startsWith :: String -> String -> Maybe String
startsWith s t = case (s, t) of
  -- Empty string is a prefix any string.
  ("", t') -> Just t'
  -- Empty string cannot start with any prefix (other than empty prefix, which
  -- is handled in the above case).
  (s', "") -> Nothing
  -- Otherwise, if first characters match, keep trying, otherwise fail.
  (s:s', t:t') -> if s == t
    then startsWith s' t'
    else Nothing
