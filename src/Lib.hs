module Lib
    where

-- | Enum with all the Planet types
data Planet = Ferengi | Betasoide | Vulcano deriving (Show, Eq)

-- | Representations of a point in space
data Point = Point { x :: Distance, y :: Distance} deriving (Show)

-- | Dummy type for angles
type Angle = Double
-- | Dummy type for days
type Day = Double
-- | Dummy type for speeds
type Speed = Double

-- | Dummy type for distances in space
type Distance = Double
-- | Dummy type for derivated of distance in space
type Distance' = Double

-- | A line is described as two points
data Line = Line { from :: Point, to :: Point}

-- | Side is useful to tell to which side of a line is a point
data Side = Above | Below deriving (Show, Eq)

-- | Slope is useful to tell if a distance is growing or shrinking
data Slope = Descending | Flat | Ascending deriving (Show, Eq, Ord)

-- | The weather of the planets which depends on their relative positions
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

-- | Entry point for most programs which checks the weather conditions for a given day
weatherAt :: Day -> Weather
weatherAt n
  -- If the sun is aligned with all the planets we have a drought
  | isSunAlignedWithPlanetsAt n = Drought
  -- If the sun is inside the triangle at the beginning or the end of the day
  -- then the weather is rain (possibly heavy rain)
  | isSunInsideTriangleAt n || isSunInsideTriangleAt (n+1) =
    -- If the slope at the beginning of the day was ascending and it's descending
    -- by the end then we have a peek of rain
    if slopeOfTriangleAt n == Ascending && slopeOfTriangleAt (n+1) == Descending  then
      HeavyRain
    else
      Rain
  -- If planet A was at one side of the other planets and at the end of
  -- the day it's on the other side then that day the planets were aligned
  | sideOfOtherPlanet n /= sideOfOtherPlanet (n+1) &&
    not (isSunAlignedWithPlanetsAt (n+1)) = Perfect
  -- otherwise we don't know what the weather will look like
  | otherwise = Unknown

sideOfOtherPlanet :: Day -> Maybe Side
sideOfOtherPlanet n = sideOf (positionAfterDays n Vulcano)
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

-- | Using the angles check if the sun is inside the triangle
-- if for each planet the other two planets are on different sides of the sun
-- then the sun must be inside the triangle that all planets form
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

-- | Check if the angles of planets are all aligned with the sun
-- If mod 180 all angles are the same then all planets are aligned
isSunAlignedWithPlanets :: Angle -> Angle -> Angle -> Bool
isSunAlignedWithPlanets a1 a2 a3 =
  (a1 `pmod` 180) == (a2 `pmod` 180) && (a2 `pmod` 180) == (a3 `pmod` 180)

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

-- | Check to which side of a line a point is
--
-- If the point is on the line then return `Nothing`
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
  in if | abs tmp <= 1 -> Nothing
        | tmp < 0 -> Just Below
        | otherwise -> Just Above


distanceBetween :: Point -> Point -> Distance
distanceBetween p1 p2 =
  let
    x' = x p2 - x p1
    y' = y p2 - y p1
  in sqrt $ x' ^ 2 + y' ^ 2

allPlanets :: [Planet]
allPlanets = [Vulcano, Ferengi, Betasoide]

otherPlanets :: Planet -> [Planet]
otherPlanets Ferengi = [Betasoide, Vulcano]
otherPlanets Betasoide = [Vulcano, Ferengi]
otherPlanets Vulcano = [Betasoide, Ferengi]

distanceBetweenPlanets :: Day -> Planet -> Planet -> Distance
distanceBetweenPlanets day p1 p2 =
  let
    v = toRadians $ speed p1 - speed p2
    d1 = distanceFromTheSun p1
    d2 = distanceFromTheSun p2
  in
    sqrt $ (d1 ^ 2) + (d2 ^ 2) - (2 * d1 * d2 * cos (day * v))

distanceBetweenPlanets' :: Day -> Planet -> Planet -> Distance'
distanceBetweenPlanets' day p1 p2 =
  let
    v = toRadians $ speed p1 - speed p2
    d1 = distanceFromTheSun p1
    d2 = distanceFromTheSun p2
  in (d1*d2*v*sin(v*day))
     /
     sqrt ((-2)*d1*d2*cos(v*day)+d1^2+d2^2)

distanceBetweenAllPlanets :: Day -> Distance
distanceBetweenAllPlanets n =
  distanceBetweenPlanets n Vulcano Ferengi +
  distanceBetweenPlanets n Vulcano Betasoide +
  distanceBetweenPlanets n Ferengi Betasoide

slopeOfTriangleAt :: Day -> Slope
slopeOfTriangleAt n =
  let
    d = distanceBetweenAllPlanets' n
  in
    if | abs d < 1 -> Flat
       | d < 0 -> Descending
       | otherwise -> Ascending

distanceBetweenAllPlanets' :: Day -> Distance
distanceBetweenAllPlanets' n =
  distanceBetweenPlanets' n Vulcano Ferengi +
  distanceBetweenPlanets' n Vulcano Betasoide +
  distanceBetweenPlanets' n Ferengi Betasoide

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

-- | Get the angle of a planet at a given day
positionAfterDays :: Day -> Planet -> Point
positionAfterDays day planet =
  let
    angle = toRadians $ angleAfterDays day planet
    h = distanceFromTheSun planet
  in Point { x = cos angle * h, y = sin angle * h }

-- | Get the angle of a planet at a given day
angleAfterDays :: Day -> Planet -> Angle
angleAfterDays day planet = (day * speed planet) `pmod` 360

-- | Get the distance from the Sun of a given planet
distanceFromTheSun :: Planet -> Distance
distanceFromTheSun Ferengi = 500000
distanceFromTheSun Betasoide = 2000000
distanceFromTheSun Vulcano = 1000000

-- | Get the speed of a given planet
speed :: Planet -> Speed
speed Ferengi = 1
speed Betasoide = 3
speed Vulcano = -5
