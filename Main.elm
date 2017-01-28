import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard
import Random
import Dict
import Set
import Time

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
  { levels : List Level.Level
  , gravity : KDir
  , gameOver : Bool
  , levelCleared : Bool
  , stable : Bool
  }


getLevel : Model -> Level.Level
getLevel model =
  Maybe.withDefault LevelsDB.emptyLevel (List.head model.levels)

init : (Model, Cmd Msg)
init =
  ({ levels = LevelsDB.levels
   , gravity = Down
   , gameOver = False
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

updateGameOver : Level -> Bool
updateGameOver level =
  Level.playerBlok level |> andThenPred blokInBounds |> not

updateLevels : Bool -> Level.Level -> List Level.Level -> List Level.Level
updateLevels isCleared newLevel levels =
  let
    tail = Maybe.withDefault [] (List.tail levels)
  in
      if isCleared then
        tail
      else
        newLevel :: tail

updateStat : Model -> Model
updateStat model =
  let
    newLevel =
      Physics.updateLevel model.gravity (getLevel model)
    newStable =
      (getLevel model) == newLevel
    newGameOver =
      updateGameOver newLevel
    levelCleared =
      Physics.levelCleared newLevel
    newLevels =
      updateLevels levelCleared newLevel model.levels
  in
    { model
    | levels = newLevels
    , gameOver = newGameOver
    , stable = newStable
    , levelCleared = levelCleared
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick x ->
      (updateStat model, Cmd.none)
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


view : Model -> Html Msg
view model =
    Html.div []
      [
        Svg.svg [ viewBox "0 0 100 100", width "700px" ]
        [ viewBackGround
        , Svg.g [Svg.Attributes.transform "translate(10,10) scale(0.8)"] (Level.viewLevel (getLevel model))
        , Svg.text_ [x "10", y "5", fontSize "7", fill "#ffffff"] [Svg.text (if model.gameOver then "game over" else "")]
        ]
      ]
