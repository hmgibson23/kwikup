{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module BowerNpm where

import Data.Aeson
import Data.Functor
import qualified Data.Attoparsec.Text as A
import Data.Attoparsec.Text (Parser)
import Data.String.Utils (rstrip)
import Data.List.Split (splitOn)
import Data.List (isInfixOf)
import Data.Char (isSpace)
import System.Process
import Control.Monad
import qualified Data.ByteString.Char8 as BS
import GHC.Generics
import GHC.IO.Exception (ExitCode (ExitFailure, ExitSuccess))
import Data.Map (Map, (!), toList)

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

-- TODO: isMVersion is crude and unwieldy
compareMVersions :: String -> String -> Bool
compareMVersions x y = isMVersion x && listCompare (l x) (l y)
  where l = splitOn "."
        isMVersion p = length (l p) > 2

-- HEINOUS - There must be a better way but I can't
-- be bothered to think about it now
listCompare :: [String] -> [String] -> Bool
listCompare x y = let h = head x
                      j = head y
                      k = x !! 1
                      l = y !! 1
                      m = x !! 2
                      n = y !! 2
                  in ((x < y) || (k < l) || (m < n))


-- Note this could be simplified by chaining monads
checkVersion :: Monad m => (t, String) -> (t -> m (Maybe String)) -> m (Maybe (t, String))
checkVersion y f = do
  latest <- f (fst y)
  case latest of
   Just l -> if compareMVersions (tail (snd y)) l then
              return (Just (fst y, l))
            else return Nothing
   _ -> return Nothing

mapVersions :: Maybe [(String, String)] -> String -> IO [Maybe (String, String)]
mapVersions a fileName = case a of
  (Just a) -> if "bower" `isInfixOf` fileName then
               mapM (`checkVersion` getVersion bowerCommand) a
             else
               mapM (`checkVersion` getVersion nodeCommand) a
  Nothing -> return []


-- TODO: Sould probably not just join all the dependencies together as it might be
-- nicer to know where each one is missing
bowerNodeMap x fileName = mapVersions j fileName
  where j = do
          ds <- deps x devDependencies
          ps <- deps x dependencies
          return (ds ++ ps)


-- Need to short circuit on a failure
m a b = map check z
  where z = zip a b
        check p = uncurry (<) p
