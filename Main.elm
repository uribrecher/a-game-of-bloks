import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events
import Keyboard
import Random
import Dict
import Set
import Time
import Array exposing (Array)

import BaseTypes exposing (..)
import Level exposing (Level)
import Physics
import LevelsDB


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { levels : Array Level.Level
  , levelIndex : Int
  , state : Level.Level
  , gravity : KDir
  , fail : Bool
  , levelCleared : Bool
  , stable : Bool
  }


init : (Model, Cmd Msg)
init =
  (updateReplay
    { levels = Array.fromList LevelsDB.levels
    , state = LevelsDB.emptyLevel
    , levelIndex = 0
    , gravity = Down
    , fail = False
    , levelCleared = False
    , stable = False
    }, Cmd.none)


-- UPDATE

msgToDir : Msg -> Maybe KDir
msgToDir msg =
  case msg of
    KeyDown 38 -> Just Up
    KeyDown 39 -> Just Right
    KeyDown 40 -> Just Down
    KeyDown 37 -> Just Left
    _ -> Nothing

updateGravity : Msg -> Model -> Model
updateGravity msg model =
  let
    newGravity =
      if model.stable then
        Maybe.withDefault model.gravity (msgToDir msg)
      else
        model.gravity
  in
    {model | gravity = newGravity}

andThenPred : (a -> Bool) -> Maybe a -> Bool
andThenPred pred x =
  case x of
    Just v -> pred v
    Nothing -> False

isFail : Level -> Bool
isFail level =
  Level.playerBlok level |> andThenPred blokInBounds |> not

updateModelCleared : Model -> Model
updateModelCleared model =
  let
    newLevelIndex = model.levelIndex + 1
    newState =
      Maybe.withDefault LevelsDB.emptyLevel (Array.get newLevelIndex model.levels)
  in
    { model
    | levelIndex = newLevelIndex
    , levelCleared = False
    , stable = False
    , gravity = Down
    , state = newState }

updateModelNormal : Model -> Model
updateModelNormal model =
  let
    newLevelState =
      Physics.updateLevelState model.gravity model.state
    newStable =
      model.state == newLevelState
    newFail =
      isFail newLevelState
    newLevelCleared =
      Physics.levelCleared newLevelState
  in
    { model
    | fail = newFail
    , stable = newStable
    , levelCleared = newLevelCleared
    , state = newLevelState
     }

updateModel : Model -> Model
updateModel model =
  if model.levelCleared then
    updateModelCleared model
  else
    updateModelNormal model

updateReplay : Model -> Model
updateReplay model =
  { model
  | fail = False
  , state = Maybe.withDefault LevelsDB.emptyLevel (Array.get model.levelIndex model.levels)
  , gravity = Down
  , stable = False
  }


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick x ->
      (updateModel model, Cmd.none)
    Replay ->
      (updateReplay model, Cmd.none)
    _ ->
      (updateGravity msg model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      [ Keyboard.downs KeyDown
      , Keyboard.ups KeyUp
      , Time.every (50 * Time.millisecond) Tick
      ]

-- VIEW

viewBackGround : Svg Msg
viewBackGround =
  rect [ fill "#000000"
       , x "0"
       , y "0"
       , width (toString (rectSize * boundsX))
       , height (toString (rectSize * boundsY))
       ] []

viewReplayButton : Svg Msg
viewReplayButton =
    Svg.rect [
      x "2"
    , y "2"
    , width "5"
    , height "5"
    , Svg.Events.onClick Replay
    , fill "#00ff00"
    ] [Svg.path [
        stroke "#000000"
        ,d "M 1536,1280 V 832 q 0,-26 -19,-45 -19,-19 -45,-19 h -448 q -42,0 -59,40 -17,39 14,69 l 138,138 Q 969,1152 768,1152 664,1152 569.5,1111.5 475,1071 406,1002 337,933 296.5,838.5 256,744 256,640 256,536 296.5,441.5 337,347 406,278 475,209 569.5,168.5 664,128 768,128 q 169,0 304,99.5 135,99.5 185,261.5 7,23 30,23 h 199 q 16,0 25,-12 10,-13 7,-27 Q 1479,298 1370.5,161 1262,24 1104.5,-52 947,-128 768,-128 612,-128 470,-67 328,-6 225,97 122,200 61,342 0,484 0,640 q 0,156 61,298 61,142 164,245 103,103 245,164 142,61 298,61 147,0 284.5,-55.5 Q 1190,1297 1297,1196 l 130,129 q 29,31 70,14 39,-17 39,-59 z"
        , Svg.Attributes.style "fill:currentColor"] []
       ]

view : Model -> Html Msg
view model =
    Html.div []
      [
        Svg.svg [ viewBox "0 0 100 100", width "700px" ]
        [ viewBackGround
        , Svg.g [Svg.Attributes.transform "translate(10,10) scale(0.8)"] (Level.viewLevel model.state)
        , Svg.text_ [x "10", y "5", fontSize "7", fill "#ffffff"] [Svg.text (if model.fail then "Oh no!" else ":-)")]
        , viewReplayButton]
      ]
