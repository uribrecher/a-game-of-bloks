import Html exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Keyboard
import Random
import Dict
import Time

import BaseTypes exposing (..)
import Level exposing (Level)
import Physics

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { level : Level.Level
  , gravity : KDir
  , gameOver : Bool
  , stable : Bool
  }


player =
  { structure = [(10,10),(10,11),(11,10)]
  , material = Free
  }

target =
  { structure = [(10,19), (11,19),(12,19)]
  , material = Rigid
  }

blok1 =
  { structure = [(12,10),(12,11),(12,12)]
  , material = Sliding Horizontal
  }

blok2 =
  { structure = [(3,3)]
  , material = Rigid
  }

blok3 =
  { structure = [(8,8)]
  , material = Sliding Vertical
  }

blok4 =
  { structure = [(10,8), (10,9), (11,9)]
  , material = Free
  }


blokList = [(0,player), (1,target), (2,blok1), (3,blok2), (4, blok3), (5, blok4)]

init : (Model, Cmd Msg)
init =
  ({ level =
      { bloks = Dict.fromList blokList
      , player = 0
      , target = 1
      , name = "Test123"
      }
   , gravity = Down
   , gameOver = False
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

updateStat : Model -> Model
updateStat model =
  let
    newLevel =
      Physics.updateLevel model.gravity model.level
    newStable =
      model.level == newLevel
    newGameOver =
      updateGameOver newLevel
  in
    {model | level = newLevel, gameOver = newGameOver, stable = newStable}

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

view : Model -> Html Msg
view model =
    Html.div []
      [ Level.viewLevel model.level
      , Html.text (if model.gameOver then "game over" else "running")
      ]
