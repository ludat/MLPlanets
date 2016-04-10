module Lib
    where

data Planet = Ferengi | Betasoide | Vulcano deriving (Show, Eq)

data Point = Point { x :: Distance, y :: Distance} deriving (Show)

type Region = Point -> Bool

type Angle = Double
type Day = Double
type Speed = Double

type Distance = Double
type Distance' = Double


data Weather = Drought
             | HeavyRain
             | Rain
             | Perfect
             | Unknown deriving (Eq)

instance Show Weather where
  show Drought    = "That day the weather will be: drougth"
  show Rain       = "That day the weather will be: normal rain"
  show HeavyRain  = "That day the weather will be: heavy rain"
  show Perfect    = "That day the weather will be: optimal conditions"
  show Unknown    = "I don't know how the weather will be that day"

weatherAt :: Day -> Weather
weatherAt n
  | isSunAlignedWithPlanetsAt n = Drought
  | isSunInsideTriangleAt n || isSunInsideTriangleAt (n+1) =
    if slopeOfTriangleAt n == Ascending && slopeOfTriangleAt (n+1) == Descending  then
      HeavyRain
    else
      Rain
  | (sideOfOtherPlanet n) /= (sideOfOtherPlanet (n+1)) &&
    not (isSunAlignedWithPlanetsAt (n+1)) = Perfect
  | otherwise = Unknown

sideOfOtherPlanet :: Day -> Maybe Side
sideOfOtherPlanet n = sideOf (positionAfterDays n Vulcano) $
                         Line { from = positionAfterDays n Ferengi
                              , to = positionAfterDays n Betasoide
                              }


pmod' :: (Num a, Ord a) => a -> a -> a
pmod' i n
  | i < 0 = until (>= 0) (+ n) i
  | i >= n = until (< n) (subtract n) i
  | otherwise = i



pmod :: Angle -> Angle -> Angle
pmod = pmod'

-- myTrace a b = trace (a ++ show b) b

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

data Line = Line { from :: Point, to :: Point}

data Side = Above | Below deriving (Show, Eq)

isSunInsideTriangleAt :: Day -> Bool
isSunInsideTriangleAt n = isSunInsideTriangle
                              (angleAfterDays n Ferengi)
                              (angleAfterDays n Betasoide)
                              (angleAfterDays n Vulcano)

isSunAlignedWithPlanetsAt :: Day -> Bool
isSunAlignedWithPlanetsAt n =
  let
    a1 = angleAfterDays n Vulcano
    a2 = angleAfterDays n Ferengi
    a3 = angleAfterDays n Betasoide
  in isSunAlignedWithPlanets a1 a2 a3

sideOf :: Point -> Line -> Maybe Side
sideOf p l =
  let
    l_p1_x = x $ from l
    l_p2_x = x $ to l
    l_p1_y = y $ from l
    l_p2_y = y $ to l
    p_x = x p
    p_y = y p
    tmp = (l_p2_x - l_p1_x)*(p_y - l_p1_y) - (l_p2_y - l_p1_y)*(p_x - l_p1_x);
  in if abs tmp <= 1 then
       Nothing
     else if tmp < 0 then
       Just Below
     else
       Just Above


distanceBetween :: Point -> Point -> Distance
distanceBetween p1 p2 =
  let
    x' = (x p2) - (x p1)
    y' = (y p2) - (y p1)
  in sqrt (x' ^ 2 + y' ^ 2)

allPlanets :: [Planet]
allPlanets = [Vulcano, Ferengi, Betasoide]

otherPlanets :: Planet -> [Planet]
otherPlanets Ferengi = [Betasoide, Vulcano]
otherPlanets Betasoide = [Vulcano, Ferengi]
otherPlanets Vulcano = [Betasoide, Ferengi]

distanceBetweenPlanets :: Day -> Planet -> Planet -> Distance
distanceBetweenPlanets day p1 p2 =
  let
    v1 = toRadians $ speed p1
    v2 = toRadians $ speed p2
    d1 = distanceFromTheSun p1
    d2 = distanceFromTheSun p2
  in
    sqrt $ (d1 ^ 2) + (d2 ^ 2) - (2 * d1 * d2 * cos (day * (v1 - v2)))

distanceBetweenPlanets' :: Day -> Planet -> Planet -> Distance'
distanceBetweenPlanets' day p1 p2 =
  let
    v1 = toRadians $ speed p1
    v2 = toRadians $ speed p2
    d1 = distanceFromTheSun p1
    d2 = distanceFromTheSun p2
  in (d1*d2*(v1-v2)*sin((v1-v2)*day))
     /
     sqrt ((-2)*d1*d2*cos((v1-v2)*day)+d1^2+d2^2)

distanceBetweenAllPlanets :: Day -> Distance
distanceBetweenAllPlanets n =
  (distanceBetweenPlanets n Vulcano Ferengi) +
  (distanceBetweenPlanets n Vulcano Betasoide) +
  (distanceBetweenPlanets n Ferengi Betasoide)

slopeOfTriangleAt :: Day -> Slope
slopeOfTriangleAt n =
  let
    d = distanceBetweenAllPlanets' n
  in
    if abs d < 1 then
      Flat
    else if d < 0 then
      Descending
    else
      Ascending

distanceBetweenAllPlanets' :: Day -> Distance
distanceBetweenAllPlanets' n =
  (distanceBetweenPlanets' n Vulcano Ferengi) +
  (distanceBetweenPlanets' n Vulcano Betasoide) +
  (distanceBetweenPlanets' n Ferengi Betasoide)

data Slope = Descending | Flat | Ascending deriving (Show, Eq, Ord)

distanceBetweenPlanets2 :: Day -> Planet -> Planet -> Distance
distanceBetweenPlanets2 day p1 p2 =
  let
    pos1 = positionAfterDays day p1
    pos2 = positionAfterDays day p2
  in distanceBetween pos1 pos2

toRadians :: Angle -> Angle
toRadians n = n * pi / 180

toDegrees :: Angle -> Angle
toDegrees n = n * 180 / pi

positionAfterDays :: Day -> Planet -> Point
positionAfterDays day planet =
  let
    angle = toRadians $ angleAfterDays day planet
    h = distanceFromTheSun planet
  in Point { x = (cos angle) * h, y = (sin angle) * h }

angleAfterDays :: Day -> Planet -> Angle
angleAfterDays day planet = (day * (speed planet)) `pmod` 360

distanceFromTheSun :: Planet -> Distance
distanceFromTheSun Ferengi = 500000
distanceFromTheSun Betasoide = 2000000
distanceFromTheSun Vulcano = 1000000

speed :: Planet -> Speed
speed Ferengi = 1
speed Betasoide = 3
speed Vulcano = -5

-- HERE BE DRAGONS


-- distanceBetweenPlanets' :: Day -> Planet -> Planet -> Distance
-- distanceBetweenPlanets' day p1 p2 =
--   let
--     angle1 = (toRadians $ angleAfterDays day p1)
--     angle2 = (toRadians $ angleAfterDays day p2)
--     d1 = distanceFromTheSun p1
--     d2 = distanceFromTheSun p2
--   in (d1 ^ 2) + (d2 ^ 2) - 2 * d1 * d2 * cos (abs (angle1 - angle2))
