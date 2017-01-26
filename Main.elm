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
import LevelBuilder

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
  , levelCleared : Bool
  , stable : Bool
  }


player =
  { structure = Set.fromList [(10,10),(10,11),(11,10)]
  , material = Free
  }

target =
  { structure = Set.fromList [(10,7), (11,7),(12,7)]
  , material = Rigid
  }

blok1 =
  { structure = Set.fromList [(12,10),(12,11),(12,12)]
  , material = Sliding Horizontal
  }

blok2 =
  { structure = Set.fromList [(3,3)]
  , material = Rigid
  }

blok3 =
  { structure = Set.fromList [(8,8)]
  , material = Sliding Vertical
  }

blok4 = LevelBuilder.frame Rigid (0,0) (20,20)

blokList = [(0,player), (1,target), (2,blok1), (3,blok2) , (4, blok3), (5, blok4)]

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

updateStat : Model -> Model
updateStat model =
  let
    newLevel =
      Physics.updateLevel model.gravity model.level
    newStable =
      model.level == newLevel
    newGameOver =
      updateGameOver newLevel
    levelCleared =
      Physics.levelCleared newLevel
  in
    {model | level = if model.levelCleared then model.level else newLevel
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

view : Model -> Html Msg
view model =
    Html.div []
      [ Level.viewLevel model.level
      , Html.text (if model.gameOver then "game over" else "running")
      ]
