module BaseTypes exposing (Blok, BlokDict, KDir(..), Loc, MDir(..), Material(..), Msg(..), blokInBounds, boundsX, boundsY, emptyBlok, keyDecoder, locInBounds, locToEmptyRect, locToRect, materialColor, rectSize, viewBlok, viewEmptyBlok)

import Dict
import Json.Decode
import Set exposing (Set)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


keyDecoder : Json.Decode.Decoder KDir
keyDecoder =
    Json.Decode.map toDirection (Json.Decode.field "key" Json.Decode.string)


toDirection : String -> KDir
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


type KDir
    = Up
    | Down
    | Left
    | Right
    | Other


type MDir
    = Horizontal
    | Vertical


type Material
    = Rigid
    | Sliding MDir
    | Free


type alias Loc =
    ( Int, Int )


type alias Blok =
    { structure : Set Loc
    , material : Material
    }


emptyBlok : Blok
emptyBlok =
    { structure = Set.empty
    , material = Rigid
    }


type alias BlokDict =
    Dict.Dict Int Blok


type Msg
    = KeyDown KDir
    | KeyUp KDir
    | Tick Time.Posix
    | Replay


boundsX =
    20


boundsY =
    20


rectSize =
    5


locInBounds : Loc -> Bool
locInBounds ( x, y ) =
    x >= 0 && y >= 0 && x < boundsX && y < boundsY


blokInBounds : Blok -> Bool
blokInBounds blok =
    Set.foldl (\x accum -> accum || locInBounds x) False blok.structure


materialColor : Material -> String
materialColor material =
    case material of
        Rigid ->
            "#0000ff"

        Free ->
            "#ff0000"

        Sliding _ ->
            "#00ff00"


locToRect : Material -> Loc -> Svg Msg
locToRect material ( xv, yv ) =
    rect
        [ fill (materialColor material)
        , x (String.fromInt (xv * rectSize))
        , y (String.fromInt (yv * rectSize))
        , width (String.fromInt rectSize)
        , height (String.fromInt rectSize)
        ]
        []


locToEmptyRect : String -> Loc -> Svg Msg
locToEmptyRect colorStr ( xv, yv ) =
    rect
        [ fillOpacity "0%"
        , stroke colorStr
        , x (String.fromInt (xv * rectSize))
        , y (String.fromInt (yv * rectSize))
        , width (String.fromInt rectSize)
        , height (String.fromInt rectSize)
        ]
        []


viewBlok : Blok -> List (Svg Msg)
viewBlok blok =
    Set.toList blok.structure |> List.map (locToRect blok.material)


viewEmptyBlok : String -> Blok -> List (Svg Msg)
viewEmptyBlok color blok =
    Set.toList blok.structure |> List.map (locToEmptyRect color)
