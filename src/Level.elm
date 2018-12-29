module Level exposing (Level, playerBlok, targetBlok, viewLevel)

import BaseTypes exposing (..)
import Dict
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias Level =
    { bloks : BlokDict
    , player : Int
    , target : Int
    , name : String
    }


playerBlok : Level -> Maybe Blok
playerBlok level =
    Dict.get level.player level.bloks


targetBlok : Level -> Maybe Blok
targetBlok level =
    Dict.get level.target level.bloks


andThenList : (a -> List b) -> Maybe a -> List b
andThenList callback r =
    case r of
        Just x ->
            callback x

        Nothing ->
            []



-- VIEW --


viewPlayer : Level -> List (Svg Msg)
viewPlayer level =
    playerBlok level |> andThenList (viewEmptyBlok "#ffff00")


viewTarget : Level -> List (Svg Msg)
viewTarget level =
    targetBlok level |> andThenList (viewEmptyBlok "#00ffff")


viewLevel : Level -> List (Svg Msg)
viewLevel level =
    List.concat
        [ [ viewFrame ]
        , List.concatMap viewBlok (Dict.values level.bloks)
        , viewTarget level
        , viewPlayer level
        , [ Svg.text_ [ x "0", y "105", fontSize "5", fill "#ffffff" ] [ Svg.text level.name ] ]
        ]


viewFrame : Svg Msg
viewFrame =
    rect
        [ fillOpacity "0%"
        , stroke "#ffffff"
        , x "0"
        , y "0"
        , width "100"
        , height "100"
        ]
        []



-- BEGIN TODO: use HTTP GET to retrieve that from the server
-- convert JSON to these structures
{--
JSON EXAMPLE

{
  "objects": [
    {
      "structure": [[1,1],[2,1],[3,1]]
      ,"material": "Rigid"
    },
    {
      "structure": [[10,10]]
      ,"material": "Free"
    },
    {
      "structure": [[5,1],[5,2],[5,3]]
      ,"material": "Sliding Horizontal"
    }
  ]
}

--}
