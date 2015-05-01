{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BowerNpm where

import           Control.Applicative
import           Data.Aeson
import qualified Data.Attoparsec.ByteString.Char8 as A
import           Data.Attoparsec.Internal.Types (Parser)
import qualified Data.ByteString.Char8 as BS
import           Data.Char (isSpace)
import           Data.List (isInfixOf)
import           Data.Map (Map, toList)
import           Data.String.Utils (rstrip)
import           GHC.Generics
import           GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import           System.Process


data Dependencies =
  Dependencies {
    devDependencies :: Dependency,
    dependencies :: Dependency
    }
  deriving (Show, Generic)

instance FromJSON Dependencies

newtype Dependency = Dependency (Map String String)
                   deriving Show

instance FromJSON Dependency where
  parseJSON val = Dependency <$> parseJSON val


data Mversion = Mversion {
  major :: Integer,
  minor :: Integer,
  patch :: Integer
  } deriving (Show)

instance Eq Mversion where
  a == b = (major a == major b) && (minor a == minor b) && (patch a == patch b)

instance Ord Mversion where
  a < b = (major a < major b) || (minor a < minor b) || (patch a < patch b)
  a <= b = a == b || a < b

--mversion :: Parser Mversion
mversion :: Data.Attoparsec.Internal.Types.Parser BS.ByteString Mversion
mversion = do
  major <- A.decimal
  A.char '.'
  minor <- A.decimal
  A.char '.'
  patch <- A.decimal
  return $ Mversion major minor patch

parseMversion :: String -> Either String Mversion
parseMversion x = A.parseOnly mversion (BS.pack x)

isMversion :: String -> Bool
isMversion x = case parseMversion x of
  (Right _) -> True
  _ ->  False

dep :: Dependency -> Map String String
dep (Dependency x) = x

deps :: Functor f => f a -> (a -> Dependency) -> f [(String, String)]
deps x accessor = f . y $ fmap accessor x
  where y = fmap dep
        f = fmap toList

getVersion :: Monad m => (t -> m (ExitCode, String, c)) -> t -> m (Maybe String)
getVersion c y =  do
  res <- c y
  if checkExitCode res == 0 then
    return $ Just (rstrip (getStdOut res))
    else
    return Nothing

nodeCommand :: String -> IO (ExitCode, String, String)
nodeCommand y = readProcessWithExitCode "npm" ["show", y, "version"] []

bowerCommand :: String -> IO (ExitCode, String, String)
bowerCommand y = readProcessWithExitCode "bower" ["info", y] []

checkExitCode :: (ExitCode, a, b) -> Int
checkExitCode (ExitFailure a, _, _) = a
checkExitCode (ExitSuccess, _, _) = 0

getStdOut :: (a, b, c) -> b
getStdOut (_, a, _) = a

prepareString :: String -> BS.ByteString
prepareString = stripped
  where stripped res = BS.dropWhile (\ x -> isSpace x || x == '-') (version res)
        version res = head (tail (BS.split '\n' (snd (BS.breakSubstring "Available versions"
                                                        (BS.pack res)))))

compareMVersions :: String -> String -> Bool
compareMVersions x y = isMversion x && l < p
  where l = parseMversion x
        p = parseMversion y

-- Note this could be simplified by chaining monads
checkVersion :: Monad m => (t, String) -> (t -> m (Maybe String)) -> m (Maybe (t, String))
checkVersion y f = do
  latest <- f (fst y)
  case latest of
   Just l -> if compareMVersions (tail (snd y)) l then
              return (Just (fst y, l))
            else return Nothing
   _ -> return Nothing


-- could use mapConcurrently here to get things to work
mapVersions :: Maybe [(String, String)] -> String -> IO [Maybe (String, String)]
mapVersions a fileName = case a of
  (Just x) -> if "bower" `isInfixOf` fileName then
               mapM (`checkVersion` getVersion bowerCommand) x
             else
               mapM (`checkVersion` getVersion nodeCommand) x
  Nothing -> return []


bowerNodeMap :: Maybe Dependencies -> String -> IO [Maybe (String, String)]
bowerNodeMap x fileName = mapVersions j fileName
  where j = do
          ds <- deps x devDependencies
          ps <- deps x dependencies
          return (ds ++ ps)
