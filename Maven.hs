{-# LANGUAGE FlexibleContexts, NoMonomorphismRestriction, OverloadedStrings #-}


module Maven where

import Text.XML.HXT.Core
import Text.HandsomeSoup
import Text.Read
import Data.Tree.NTree.TypeDefs
import qualified Data.ByteString.Char8 as BS
import Text.Parsec.Char (digit, char, oneOf, noneOf, string)
import Text.Parsec.Combinator (many1, between, sepBy, endBy, optional, skipMany1)
import Text.Parsec.String (Parser)
import Text.Parsec.Error (ParseError)
import Text.Parsec.Prim
import Control.Monad
import Control.Applicative ((<*),(*>))

data Dependency = Dependency {
  prefix :: String,
  name :: String,
  version :: Version
  } deriving Show

data Version = Version {
  major :: Integer,
  minor :: Integer
  } deriving Show

whitespace :: Parser ()
whitespace = void $ many $ oneOf " \n\t"

stringLiteral :: Parser String
stringLiteral = s <|> many (noneOf "\\")
  where s = between (char '"') (char '"') $ many (noneOf "\"")


dependencyParser :: Parser Dependency
dependencyParser = do
  optional whitespace
  p  <- stringLiteral <* whitespace <* skipMany1 (char '%') <* whitespace
  n <- stringLiteral <* whitespace <* skipMany1 (char '%') <* whitespace
  v <- versionParser
  return $ Dependency p n v

versionParser :: Parser Version
versionParser = do
  m <- char '"' *> many1 digit <* char '.'
  mi <- many1 digit <* (char '"' <|> char '.')
  optional (many1 digit)
  return $ Version (read m) (read mi)

parseT :: Parser a -> String -> Either ParseError a
parseT p = parse p ""


--depdencyCheck :: Dependency -> Maybe Bool
dependencyCheck d = getVersions url
  where url = searchUrl ++ prefix d ++ "/" ++ name d

searchUrl :: String
searchUrl = "http://mvnrepository.com/artifact/"

rsHelper :: String -> IOStateArrow s b XmlTree
rsHelper = readString [withParseHTML yes, withWarnings no]

-- amazingly this will return a list of versions
-- with head being the first version
links :: ArrowXml cat => cat a (NTree XNode) -> cat a String
links doc = doc >>> css ".vbtn" //> getText

getVersions :: String -> IOSLA (XIOState ()) a String
getVersions url = links $ fromUrl url

openFile :: FilePath -> IO String
openFile x = do
  f <- BS.readFile x
  return (BS.unpack $ snd (BS.breakSubstring "libraryDependencies" f))

fullDeps :: Parser [[Dependency]]
fullDeps = endBy deps (char ')')

deps :: Parser [Dependency]
deps = dependencyParser `sepBy` char ','

libDeps :: Parser [Dependency]
libDeps = string "libraryDependencies ++= Seq(" <* whitespace *> deps


testParser = openFile "build.sbt" >>=
             \ x -> return $ parseT libDeps x
