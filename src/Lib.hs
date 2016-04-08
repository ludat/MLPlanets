module Lib
    ( pmod,
      isSunInsideTriangle,
      isSunAlignedWithPlanets,
      positionAfterDays,
      Planet(..)
      -- Region,
      -- Point
    ) where

import Debug.Trace

-- data Point = Point { x :: Integer, y :: Integer}

-- type Region = Point -> Bool

type Angle = Int

pmod :: Integral a => a -> a -> a
pmod i n = until (>= 0) (+ n) (i `mod` n)

myTrace a b = trace (a ++ show b) b

isSunInsideTriangle :: Angle -> Angle -> Angle -> Bool
isSunInsideTriangle a1 a2 a3 =
   (
     (
       ((a2 - a1) `pmod` 360 <= 180) &&
       ((a3 - a1) `pmod` 360 >= 180)
     ) ||
     (
       ((a2 - a1) `pmod` 360 >= 180) &&
       ((a3 - a1) `pmod` 360 <= 180)
     )
   ) &&
   (
     (
       ((a1 - a2) `pmod` 360 <= 180) &&
       ((a3 - a2) `pmod` 360 >= 180)
     ) ||
     (
       ((a1 - a2) `pmod` 360 >= 180) &&
       ((a3 - a2) `pmod` 360 <= 180)
     )
   )

isSunAlignedWithPlanets :: Angle -> Angle -> Angle -> Bool
isSunAlignedWithPlanets a1 a2 a3 =
  (a1 `pmod` 180) == (a2 `pmod` 180) && (a2 `pmod` 180) == (a3 `pmod` 180)

areTrianglesAligned = undefined

data Planet = Ferengi | Betasoide | Vulcano deriving Show

allPlanets :: [Planet]
allPlanets = [Vulcano, Ferengi, Betasoide]


speed :: Planet -> Int
speed Ferengi = 1
speed Betasoide = 3
speed Vulcano = -5

positionAfterDays days planet = (days * (speed planet)) `pmod` 360

newtype Distance = Distance Int deriving (Show)

distanceFromTheSun :: Planet -> Distance
distanceFromTheSun Ferengi = Distance 500000
distanceFromTheSun Betasoide = Distance 2000000
distanceFromTheSun Vulcano = Distance 1000000
