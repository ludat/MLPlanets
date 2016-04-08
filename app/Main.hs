module Main where

import Lib

main :: IO ()
main = do
  print $ filter (\x -> isSunInsideTriangle
                              (positionAfterDays x Ferengi)
                              (positionAfterDays x Betasoide)
                              (positionAfterDays x Vulcano)) [0..180]
