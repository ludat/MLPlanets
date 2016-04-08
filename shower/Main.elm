import Debug exposing (..)
import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Text exposing (fromString)
import Time exposing (..)

type alias Speed = Float


main : Signal Element
main =
  Signal.map clock (every millisecond)

distanceBetween : (Float, Float) -> (Float, Float) -> Float
distanceBetween (x1,y1) (x2,y2) =
  let
    x = x2 - x1
    y = y2 - y1
  in sqrt (x ^ 2 + y ^ 2)

distanceBetweenPlanets : Time -> Planet -> Planet -> Float
distanceBetweenPlanets t p1 p2 =
  distanceBetween (positionOfPlanet p1 t) (positionOfPlanet p2 t)

clock : Float -> Element
clock t =
  let
    mult = 1
    t' = t
       |> inSeconds
       |> (\x -> x * mult)
       |> (\x -> x - toFloat (floor x))
    t = t
      |> inSeconds
      |> (\x -> x * 1)
      |> floor
      |> (\x -> x % 180)
      |> toFloat
      |> (\x -> x + t')
    -- t = 107.5
  in
    collage 700 700
      ([ circle 15 |> filled yellow
      ] ++
      (List.map (drawPlanetAndOrbit t) allPlanets) ++
      (List.map (drawSunPlanetLine t) allPlanets) ++
      [ drawTriangle t
      , (((leftAligned <| fromString "Perimeter of the Triangle") `beside` (show (perimeterAt t)))
        `above` ((leftAligned <| fromString "Day :") `beside` (show t) ))
          |> toForm |> move (0, 300)
      ])

perimeterAt t =
  (distanceBetweenPlanets t Vulcano Betasoide) + (distanceBetweenPlanets t Vulcano Ferengi) + (distanceBetweenPlanets t Betasoide Ferengi)
sun : (Float, Float)
sun = (0,0)

type Planet = Ferengi | Betasoide | Vulcano

drawSunPlanetLine t planet = path [(0,0), positionOfPlanet planet t] |> traced defaultLine

allPlanets : List Planet
allPlanets = [Vulcano, Ferengi, Betasoide]

drawTriangle t =
  path ((List.map (flip positionOfPlanet t) allPlanets) ++ [positionOfPlanet Vulcano t]) |> traced defaultLine

speedOf : Planet -> Float
speedOf planet =
  case planet of
    Ferengi -> 1
    Betasoide -> 3
    Vulcano -> -5

colorOf : Planet -> Color
colorOf planet =
  case planet of
    Ferengi -> green
    Betasoide -> blue
    Vulcano -> red

-- positionAfterDays days planet = (days * (speed planet)) `pmod` 360

type alias Distance = Float

positionOfPlanet : Planet -> Time -> (Float, Float)
positionOfPlanet planet t =
  let
    len = distanceFromTheSun planet
    speed = speedOf planet
    angle = degrees (t * (speed))
  in
    fromPolar (len,angle)

drawPlanetAndOrbit : Time -> Planet -> Form
drawPlanetAndOrbit t planet =
  let
    pos = positionOfPlanet planet t
  in
    group
      [ circle (distanceFromTheSun planet) |> outlined defaultLine
      , drawPlanet planet t |> move pos
      ]

distanceFromTheSun : Planet -> Distance
distanceFromTheSun planet =
  case planet of
    Ferengi -> 50
    Betasoide -> 200
    Vulcano -> 100

drawPlanet : Planet -> Time -> Form
drawPlanet planet time =
  let
    clr = colorOf planet
  in
    circle 10 |> filled clr
