module Main where

import Lib
import System.Environment
import Text.Read (readMaybe)

main :: IO ()
main = do
  n <- readMaybe . head <$> getArgs :: IO (Maybe Day)
  case n of
    Nothing -> showHelp
    Just n -> putStrLn $ show $ weatherAt $ n `pmod` 360

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
