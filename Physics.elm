module Physics exposing (updateLevel)

import List
import Dict
import Set exposing (Set)
import BaseTypes exposing (..)
import Level exposing (Level)

moveUp : Loc -> Loc
moveUp (x,y) = (x,y-1)

moveDown : Loc -> Loc
moveDown (x,y) = (x,y+1)

moveLeft : Loc -> Loc
moveLeft (x,y) = (x-1,y)

moveRight : Loc -> Loc
moveRight (x,y) = (x+1,y)

moveNone : Loc -> Loc
moveNone (x,y) = (x,y)

dirTransform : KDir -> Loc -> Loc
dirTransform dir =
  case dir of
    Up -> moveUp
    Down -> moveDown
    Left -> moveLeft
    Right -> moveRight

isSliding : MDir -> KDir -> Bool
isSliding mdir gravity =
  case mdir of
    Horizontal -> gravity == Up || gravity == Down
    Vertical -> gravity == Left || gravity == Right

isAffectedByGravity : KDir -> Blok -> Bool
isAffectedByGravity gravity blok =
  case blok.material of
    Rigid -> False
    Free -> True
    Sliding x -> isSliding x gravity

flatten : BlokDict -> Set Loc
flatten bloks =
  Dict.foldl (\_ value accum -> Set.union value.structure accum) Set.empty bloks

noCollisions : BlokDict -> Int -> Blok -> Bool
noCollisions bloks index _ =
  let
    (x,xs) =
      Dict.partition (\key _ -> key == index) bloks
    flatX = flatten x
    flatXS = flatten xs
  in
    Set.intersect flatX flatXS |> Set.isEmpty

{-}
inBoundsAndNoCollision : BlokDict -> Int -> Blok -> Bool
inBoundsAndNoCollision bloks key val =
  blokInBounds val && hasCollisions bloks key
--}

updateBlok : KDir -> Int -> Blok -> Blok
updateBlok gravity _ blok =
    {blok | structure = Set.map (dirTransform gravity) blok.structure}

getCollisions : BlokDict -> BlokDict -> BlokDict
getCollisions live candidate =
  (Dict.filter (noCollisions candidate) candidate) |> Dict.diff live

repositionBloks : KDir -> BlokDict -> BlokDict -> BlokDict
repositionBloks gravity live dead =
    let
      candidate = Dict.union dead (Dict.map (updateBlok gravity) live)
      collision = getCollisions live candidate
    in
      if (Dict.size collision) == 0 then
        candidate
      else
        repositionBloks gravity (Dict.diff live collision) (Dict.union dead collision)

updateLevel : KDir -> Level -> Level
updateLevel gravity level =
  let
    inBoundsBloks = Dict.filter (\_ val -> blokInBounds val) level.bloks
    (liveBloks, deadBloks) = Dict.partition (\_ val -> isAffectedByGravity gravity val) inBoundsBloks
    newBloks = repositionBloks gravity liveBloks deadBloks
  in
    {level | bloks = newBloks}
