{-# LANGUAGE DeriveGeneric #-}

module BowerNpm where

import Data.Aeson
import Data.Functor
import Data.String.Utils (rstrip)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import System.Process
import Control.Monad
import GHC.Generics
import Data.Map (Map, (!), toList)

data Dependencies =
  Dependencies{
    devDependencies :: Dependency,
    dependencies :: Dependency
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
