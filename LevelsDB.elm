module LevelsDB exposing (emptyLevel, levels)

import BaseTypes exposing (..)
import LevelBuilder
import Dict exposing (Dict)

player = LevelBuilder.blok Free [(2,2)]

target = LevelBuilder.blok Rigid [(18,18)]
target2 = LevelBuilder.blok Rigid [(15,15)]
stopper1 = LevelBuilder.blok Rigid [(1,14)]
stopper2 = LevelBuilder.blok Rigid [(14,1)]

frame = LevelBuilder.frame Rigid (0,0) (20,20)

blokDict = Dict.fromList [(0,player), (1,target), (2,frame)]

pairs : List a -> List b -> List (a,b)
pairs = List.map2 (,)

bloksDict : List Blok -> Dict Int Blok
bloksDict listOfBloks =
  pairs (List.range 0 (List.length listOfBloks)) listOfBloks |> Dict.fromList

emptyLevel =
  {
    bloks = bloksDict [player, target]
  , player = 0
  , target = 1
  , name = "Oh no!!!!"
  }

finalLevel =
  { emptyLevel
  | name = "Game Completed!!!!"
  }

level1 =
  { emptyLevel
  |  bloks = bloksDict [player, target, frame]
  , name = "Hello World"
  }

level2 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, frame, stopper1]
  , name = "Hello World"
  }

level3 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, frame, stopper2]
  , name = "Hello World"
  }

levels = [level1, level2, level3, finalLevel]
