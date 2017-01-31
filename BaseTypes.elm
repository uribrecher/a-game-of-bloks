module BaseTypes exposing (..)

import Dict
import Set exposing (Set)
import Time
import Svg exposing (..)
import Svg.Attributes exposing (..)

type KDir = Up | Down | Left | Right
type MDir = Horizontal | Vertical
type Material = Rigid | Sliding MDir | Free

type alias Loc = (Int, Int)

type alias Blok =
  { structure : Set Loc
  , material : Material
  }

emptyBlok : Blok
emptyBlok =
  { structure = Set.empty
  , material = Rigid
  }

type alias BlokDict = Dict.Dict Int Blok

type Msg
  = KeyDown Int
  | KeyUp Int
  | Tick Time.Time
  | Replay

boundsX = 20
boundsY = 20
rectSize = 5

locInBounds : Loc -> Bool
locInBounds (x,y) =
  x >= 0 && y >= 0 && x < boundsX && y < boundsY

blokInBounds : Blok -> Bool
blokInBounds blok =
  Set.foldl (\x accum -> accum || locInBounds x) False blok.structure

materialColor : Material -> String
materialColor material =
  case material of
    Rigid -> "#0000ff"
    Free -> "#ff0000"
    Sliding _ -> "#00ff00"

locToRect : Material -> Loc -> Svg Msg
locToRect material (xv,yv) =
  rect [ fill (materialColor material)
       , x (toString (xv * rectSize))
       , y (toString (yv * rectSize))
       , width (toString rectSize)
       , height (toString rectSize)
       ] []

locToEmptyRect : String -> Loc -> Svg Msg
locToEmptyRect colorStr (xv,yv) =
  rect [ fillOpacity "0%"
       , stroke colorStr
       , x (toString (xv * rectSize))
       , y (toString (yv * rectSize))
       , width (toString rectSize)
       , height (toString rectSize)
       ] []

viewBlok : Blok -> List (Svg Msg)
viewBlok blok =
  Set.toList blok.structure |> List.map (locToRect blok.material)

viewEmptyBlok : String -> Blok -> List (Svg Msg)
viewEmptyBlok color blok =
  Set.toList blok.structure |> List.map (locToEmptyRect color)
