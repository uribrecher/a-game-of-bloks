module Physics exposing (updateLevel)

import List
import Dict
import Set
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

isAffectedByGravity : Material -> KDir -> Bool
isAffectedByGravity mat gravity =
  case mat of
    Rigid -> False
    Free -> True
    Sliding x -> isSliding x gravity

updateBlokStructure : KDir -> Blok -> Blok
updateBlokStructure gravity blok =
  let
    transform = if isAffectedByGravity blok.material gravity then
                  dirTransform gravity
                else
                  moveNone
  in
    {blok | structure = List.map transform blok.structure}

collisionTest : BlokDict -> Bool
collisionTest bloks =
  let
    listOfLocations = List.concatMap .structure (Dict.values bloks)
    setOfLocations = Set.fromList listOfLocations
  in
    List.length listOfLocations /= Set.size setOfLocations

updateWithCollision : KDir -> Int -> BlokDict -> BlokDict
updateWithCollision gravity index dict =
  let
    newDict = Dict.update index (Maybe.map (updateBlokStructure gravity)) dict
    collision = collisionTest newDict
  in
    if collision then
      dict
    else
      newDict

updateLevel : KDir -> Level -> Level
updateLevel gravity level =
  let
    newBloks =
      List.foldl (updateWithCollision gravity) level.bloks (Dict.keys level.bloks)
  in
    {level | bloks = newBloks}
