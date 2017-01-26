module Level exposing (Level, viewLevel, playerBlok, targetBlok)

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
    Just x -> callback x
    Nothing -> []

-- VIEW --

viewPlayer : Level -> List (Svg Msg)
viewPlayer level =
  playerBlok level |> andThenList (viewEmptyBlok "#ffff00")

viewTarget : Level -> List (Svg Msg)
viewTarget level =
  targetBlok level |> andThenList (viewEmptyBlok "#00ffff")

viewLevel : Level -> Svg Msg
viewLevel level =
  svg [ viewBox "0 0 100 100", width "700px" ]
      (viewBackGround ::
        ((List.concatMap viewBlok (Dict.values level.bloks)) ++
         viewTarget level ++ viewPlayer level))

viewBackGround : Svg Msg
viewBackGround =
  rect [ fill "#000000"
       , x "0"
       , y "0"
       , width (toString (rectSize * boundsX))
       , height (toString (rectSize * boundsY))
       ] []



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
