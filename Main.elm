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
    ] [Svg.image [x "0", y "0", width "10", height "10", xlinkHref "replay.svg"] []]


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
