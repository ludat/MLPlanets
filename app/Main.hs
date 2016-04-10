module Main where

import Lib
import System.Environment
import Data.Maybe (listToMaybe)
import Text.Read (readMaybe)

main :: IO ()
main = do
  n <- (\n -> return n >>= headMaybe >>= readMaybe) <$> getArgs :: IO (Maybe Day)
  case n of
    Nothing -> showHelp
    Just n -> putStrLn $ show $ weatherAt $ n `pmod` 360

headMaybe :: [a] -> Maybe a
headMaybe = listToMaybe

showHelp :: IO ()
showHelp = putStrLn help

help :: String
help = unlines
  [ "Help:"
  , "./mercaplanets <day>"
  , ""
  , "eg:"
  , "  ./mercaplanets 20"
  ]
