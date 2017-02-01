module LevelsDB exposing (emptyLevel, levels)

import BaseTypes exposing (..)
import LevelBuilder exposing (..)
import Dict exposing (Dict)

player = blok Free [(2,2)]
player2 = blok Free [(10,2)]
target = blok Rigid [(18,18)]
target2 = blok Rigid [(15,15)]
stopper1 = blok Rigid [(1,14)]
stopper2 = blok Rigid [(14,1)]
leftWall = vStick Rigid (19,0) 4
rightWall = union [vStick Rigid (0,0) 4, blok Rigid [(1,0)]]
midWall = vStick Rigid (16,0) 2
fram = frame Rigid (0,0) (20,20)
diode1 = diode (10,5) 10 1

openFrame = union
  [ hStick Rigid (0,0) 20
  , hStick Rigid (0,19) 20
  , vStick Rigid (0,0) 20]

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
  |  bloks = bloksDict [player, target, fram]
  , name = "Hello World"
  }

level2 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, fram, stopper1]
  , name = "use the stopper"
  }

level3 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, fram, stopper2]
  , name = "use that stopper, please"
  }

level4 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, openFrame, stopper1]
  , name = "mind the gap!"
  }

level5 =
  { emptyLevel
  |  bloks = bloksDict [player, target2, leftWall, rightWall, midWall]
  , name = "mind the gap!"
  }

level6 =
  { emptyLevel
  |  bloks = bloksDict [player2, target2, diode1, fram]
  , name = "Diode"
  }



levels = [level1, level2, level6, level3, level4, level5, finalLevel]
