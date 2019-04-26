module Parser where

import System.Environment
import System.IO
import System.Exit

import Data.Maybe (catMaybes)
import Control.Applicative

import Control.Applicative
import Data.Char

import Data.Time.Clock
import Data.Time.Format


type Identifier = String
type Declaration = (Identifier, String)
type Section = (Identifier, [Declaration])
type INIFile = [Section]


data Entry = Entry { startTime :: UTCTime
                   , endTime :: UTCTime
                   , description :: String }

newtype Parser a = P { parse :: String -> Maybe (a, String) }

item :: Parser Char
item = P (\inp -> case inp of
                  (c:cs) -> Just (c, cs)
                  _      -> Nothing)

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\c -> if p c
                   then return c
                   else empty)

digit :: Parser Char
digit = sat isDigit

alphaNum :: Parser Char
alphaNum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

instance Functor Parser where
 -- fmap :: (a -> b) -> Parser a -> Parser b
 fmap g p = P (\inp -> case parse p inp of
                         Nothing      -> Nothing
                         Just (v,out) -> Just (g v, out))


instance Applicative Parser where
  -- pure :: a -> Parser a
  pure v = P (\inp -> Just (v,inp))

  -- <*> :: Parser (a->b) -> Parser a -> Parser b
  pg <*> pa = P (\inp ->
    case parse pg inp of
      Nothing      -> Nothing
      Just (g,out) -> parse (fmap g pa) out)

string :: String -> Parser String
string []     = pure []
string (c:cs) = pure (:) <*> char c <*> string cs


instance Monad Parser where
  pa >>= f = P (\inp -> case parse pa inp of
                  Nothing -> Nothing
                  Just (a, r) -> parse (f a) r)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> Nothing)

  -- (<|>) :: Parser a -> Parser a -> Parser a
  p <|> q = P (\inp ->
    case parse p inp of
      Nothing      -> parse q inp
      Just (v,out) -> Just (v,out))

space :: Parser ()
space = fmap (\_ -> ()) (many (sat isSpace))

token :: Parser a -> Parser a
token p = space *> p <* space

symbol :: String -> Parser String
symbol ss = token (string ss)

anyCharBut :: Char -> Parser Char
anyCharBut c = sat (/= c)

iniFile :: Parser INIFile
iniFile = many section

section :: Parser Section
section = do
    header <- sectionHeader
    body <- sectionBody
    return (header, body)

sectionHeader :: Parser Identifier
sectionHeader = do
    char '['
    i <- identifier
    char ']'
    eol
    return i

sectionBody :: Parser [Declaration]
sectionBody = fmap catMaybes (many line)

line :: Parser (Maybe Declaration)
line = declaration
    <|> comment
    <|> emptyLine

emptyLine :: Parser (Maybe a)
emptyLine = pure Nothing <* eol

comment :: Parser (Maybe a)
comment = do
  char '#'
  many (anyCharBut '\n')
  eol
  return Nothing


parseEntry :: Parser Entry
parseEntry = do
      from <- timestamp
      some space
      to <- timestamp
      some space
      desc <- p_description
      return (Entry from to desc)


timestamp :: Parser UTCTime
timestamp = P (\s -> case parseTimeM True defaultTimeLocale "%F %T" s of
                       Nothing -> error "Unable to parse date"
                       Just a  -> a)

p_description :: Parser String
p_description = do
            many (anyCharBut '\n')


-- timestamp :: Parser String
-- timestamp = do
--     d <- date
--     space
--     t <- time
--     return (d <> " and " <> t)

-- date :: Parser String
-- date = do
--     year <- many digit
--     char '-'
--     month <- many digit
--     char '-'
--     day <- many digit
--     return year

-- time :: Parser String
-- time = do
--     year <- many digit
--     char ':'
--     month <- many digit
--     char ':'
--     day <- many digit
--     return (year <> month <> day)


declaration :: Parser (Maybe Declaration)
declaration = do
    i <- identifier
    space
    char '='
    space
    v <- many (anyCharBut '\n')
    eol
    return (Just (i, v))

identifier :: Parser String
identifier = do
  i <- some alphaNum
  return i

eol :: Parser ()
eol = fmap (const ()) (char '\n')
