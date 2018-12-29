module Physics exposing (levelCleared, updateLevelState)

import BaseTypes exposing (..)
import Dict
import Level exposing (Level)
import List
import Set exposing (Set)


moveUp : Loc -> Loc
moveUp ( x, y ) =
    ( x, y - 1 )


moveDown : Loc -> Loc
moveDown ( x, y ) =
    ( x, y + 1 )


moveLeft : Loc -> Loc
moveLeft ( x, y ) =
    ( x - 1, y )


moveRight : Loc -> Loc
moveRight ( x, y ) =
    ( x + 1, y )


moveNone : Loc -> Loc
moveNone ( x, y ) =
    ( x, y )


dirTransform : KDir -> Loc -> Loc
dirTransform dir =
    case dir of
        Up ->
            moveUp

        Down ->
            moveDown

        Left ->
            moveLeft

        Right ->
            moveRight

        Other ->
            moveNone


isSliding : MDir -> KDir -> Bool
isSliding mdir gravity =
    case mdir of
        Vertical ->
            gravity == Up || gravity == Down

        Horizontal ->
            gravity == Left || gravity == Right


isAffectedByGravity : KDir -> Blok -> Bool
isAffectedByGravity gravity blok =
    case blok.material of
        Rigid ->
            False

        Free ->
            True

        Sliding x ->
            isSliding x gravity


flatten : BlokDict -> Set Loc
flatten bloks =
    Dict.foldl (\_ value accum -> Set.union value.structure accum) Set.empty bloks


noCollisions : BlokDict -> Int -> Blok -> Bool
noCollisions bloks index _ =
    let
        ( x, xs ) =
            Dict.partition (\key _ -> key == index) bloks

        flatX =
            flatten x

        flatXS =
            flatten xs
    in
    Set.intersect flatX flatXS |> Set.isEmpty



{- }
   inBoundsAndNoCollision : BlokDict -> Int -> Blok -> Bool
   inBoundsAndNoCollision bloks key val =
     blokInBounds val && hasCollisions bloks key
   -
-}


updateBlok : KDir -> Int -> Blok -> Blok
updateBlok gravity _ blok =
    { blok | structure = Set.map (dirTransform gravity) blok.structure }


getCollisions : BlokDict -> BlokDict -> BlokDict
getCollisions live candidate =
    Dict.filter (noCollisions candidate) candidate |> Dict.diff live


repositionBloks : KDir -> BlokDict -> BlokDict -> BlokDict
repositionBloks gravity live dead =
    let
        candidate =
            Dict.union dead (Dict.map (updateBlok gravity) live)

        noPlayer =
            Dict.filter (\k _ -> k /= 0) candidate

        noTarget =
            Dict.filter (\k _ -> k /= 1) candidate

        noPLive =
            Dict.filter (\k _ -> k /= 0) live

        noTLive =
            Dict.filter (\k _ -> k /= 1) live

        collisionsPlayer =
            getCollisions noPLive noPlayer

        collisionsTarget =
            getCollisions noTLive noTarget

        collisions =
            Dict.union collisionsPlayer collisionsTarget
    in
    if Dict.size collisions == 0 then
        candidate

    else
        repositionBloks gravity (Dict.diff live collisions) (Dict.union dead collisions)


updateLevelState : KDir -> Level -> Level
updateLevelState gravity level =
    let
        inBoundsBloks =
            Dict.filter (\_ val -> blokInBounds val) level.bloks

        ( liveBloks, deadBloks ) =
            Dict.partition (\_ val -> isAffectedByGravity gravity val) inBoundsBloks

        newBloks =
            repositionBloks gravity liveBloks deadBloks
    in
    { level | bloks = newBloks }


levelCleared : Level -> Bool
levelCleared level =
    let
        player =
            Level.playerBlok level |> Maybe.withDefault emptyBlok |> .structure

        target =
            Level.targetBlok level |> Maybe.withDefault emptyBlok |> .structure
    in
    Set.intersect player target |> Set.isEmpty |> not
