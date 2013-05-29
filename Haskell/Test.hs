module Main (main) where

import System.Environment

import Data.OpenType


main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: test Font.otf ..."
    _ -> mapM_ doFile args


doFile :: FilePath -> IO ()
doFile filePath = do
  font <- loadFontFromFile filePath
  debugFont font

