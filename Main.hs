{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import System.Environment
import qualified Data.ByteString.Lazy as LBS
import BowerNpm


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
    res <- bowerNodeMap (decode str :: Maybe Dependencies) (head args)
    mapM_ tellOutdated res
