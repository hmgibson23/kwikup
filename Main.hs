{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}


module Main where

import Data.Aeson
import Data.Functor
import Data.String.Utils (rstrip)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import System.Process
import Control.Monad
import System.Environment
import GHC.Generics
import GHC.IO.Handle
import Data.Map (Map, (!), toList)
import qualified Data.ByteString.Lazy as LBS


data Dependencies =
  Dependencies{
    devDependencies :: Dependency
    }
  deriving (Show, Generic)

instance FromJSON Dependencies


newtype Dependency = Dependency (Map String String)
                   deriving Show

instance FromJSON Dependency where
  parseJSON val = Dependency <$> parseJSON val

dep :: Dependency -> Map String String
dep (Dependency x) = x

devDeps :: Functor f => f Dependencies -> f [(String, String)]
devDeps x = f . y $ fmap devDependencies x
  where y = fmap dep
        f = fmap toList

-- TODO: Need to do some error handling
getNodeVersion y =  do
  res <- readProcess  "npm" ["show", y, "version"] []
  return (rstrip res)

-- TODO: isMVersion is crude and unwieldy
compareMVersions :: String -> String -> Bool
compareMVersions x y = isMVersion x && listCompare (l x) (l y)
  where l = splitOn "."
        isMVersion p = length (l p) > 2

-- HEINOUS - There must be a better way but I can'p
-- be bothered to think about it now
listCompare :: [String] -> [String] -> Bool
listCompare x y = let h = head x
                      j = head y
                      k = x !! 1
                      l = y !! 1
                      m = x !! 2
                      n = y !! 2
                  in ((x < y) || (k < l) || (m < n))


checkVersion :: (String, String) -> IO (Maybe (String, String))
checkVersion y = do
  latest <- getNodeVersion (fst y)
  if compareMVersions (tail (snd y)) latest then
    -- warning - very deceptive - this is not imperative!!
     return (Just (fst y, latest))
  else return Nothing

mapVersions :: Maybe [(String, String)] -> IO [Maybe (String, String)]
mapVersions a = case a of
  (Just a) -> mapM checkVersion a
  Nothing -> return []

tellOutdated :: Maybe (String, String) -> IO ()
tellOutdated x = case x of
  (Just x) -> putStrLn (fst x ++ " has a newer version -> " ++ snd x)
  Nothing -> return ()

main :: IO ()
main = do
  args <- getArgs
  if length args < 1 then
    putStrLn "Must supply a package file"
    else do
    str <- LBS.readFile $ head args
    res <- mapVersions (devDeps (decode str :: Maybe Dependencies))
    mapM_ tellOutdated res

-- TESTS
testDeps = "{\"name\" : \"something\", \"devDependencies\" :{\"lodash\": \"3.4.1\", \"lodash-node\" : \"0.2.1\"}}"
testFile = "package.json"
testP = devDeps (decode testDeps :: Maybe Dependencies)

test = case testP of
        (Just x) -> mapM checkVersion x
        Nothing -> return []
