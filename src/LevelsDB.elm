module LevelsDB exposing (emptyLevel, levels)

import BaseTypes exposing (..)
import Dict exposing (Dict)
import LevelBuilder exposing (..)


player =
    blok Free [ ( 2, 2 ) ]


player2 =
    blok Free [ ( 11, 2 ) ]


target =
    blok Rigid [ ( 18, 18 ) ]


target2 =
    blok Rigid [ ( 15, 15 ) ]


stopper1 =
    blok Rigid [ ( 1, 14 ) ]


stopper2 =
    blok Rigid [ ( 14, 1 ) ]


leftWall =
    vStick Rigid ( 19, 0 ) 4


rightWall =
    union [ vStick Rigid ( 0, 0 ) 4, blok Rigid [ ( 1, 0 ) ] ]


midWall =
    vStick Rigid ( 16, 0 ) 2


fram =
    frame Rigid ( 0, 0 ) ( 20, 20 )


diode1 =
    diode ( 11, 5 ) 10 1


openFrame =
    union
        [ hStick Rigid ( 0, 0 ) 20
        , hStick Rigid ( 0, 19 ) 20
        , vStick Rigid ( 0, 0 ) 20
        ]


pairs : List a -> List b -> List ( a, b )
pairs =
    List.map2 Tuple.pair


bloksDict : List Blok -> Dict Int Blok
bloksDict listOfBloks =
    pairs (List.range 0 (List.length listOfBloks)) listOfBloks |> Dict.fromList


emptyLevel =
    { bloks = bloksDict [ player, target ]
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
        | bloks = bloksDict [ player, target, fram ]
        , name = "Hello World"
    }


level2 =
    { emptyLevel
        | bloks = bloksDict [ player, target2, fram, stopper1 ]
        , name = "use the stopper"
    }


level3 =
    { emptyLevel
        | bloks = bloksDict [ player, target2, fram, stopper2 ]
        , name = "use that stopper, please"
    }


level4 =
    { emptyLevel
        | bloks = bloksDict [ player, target2, openFrame, stopper1 ]
        , name = "mind the gap!"
    }



-- TODO: fix this one


level5 =
    { emptyLevel
        | bloks = bloksDict [ player, target2, leftWall, rightWall, midWall ]
        , name = "mind the gap!"
    }


level6 =
    { emptyLevel
        | bloks = bloksDict [ player2, target2, diode1, fram ]
        , name = "Diode"
    }


slid1 =
    hStick (Sliding Horizontal) ( 1, 4 ) 5


slid2 =
    vStick (Sliding Vertical) ( 10, 14 ) 3


slid3 =
    vStick (Sliding Vertical) ( 15, 17 ) 2


wall1 =
    vStick Rigid ( 11, 5 ) 14


wall2 =
    vStick Rigid ( 16, 3 ) 2


player3 =
    blok Free [ ( 3, 15 ) ]


level7 =
    { emptyLevel
        | bloks = bloksDict [ player3, target, fram, slid1, slid2, slid3, wall1, wall2 ]
        , name = "slip sliding away"
    }


player4 =
    blok Free [ ( 1, 9 ) ]


target4 =
    blok Rigid [ ( 17, 9 ) ]


mini_bloks =
    blok Rigid [ ( 1, 8 ), ( 1, 13 ), ( 9, 8 ), ( 9, 13 ), ( 13, 8 ), ( 13, 13 ) ]


wall8_1 =
    hStick Rigid ( 5, 16 ) 3


wall8_2 =
    hStick Rigid ( 9, 3 ) 3


wall8_3 =
    hStick Rigid ( 13, 16 ) 3


slid8_1 =
    vStick (Sliding Vertical) ( 6, 9 ) 7


slid8_2 =
    vStick (Sliding Vertical) ( 10, 4 ) 6


slid8_3 =
    vStick (Sliding Vertical) ( 14, 9 ) 7


reset_bloks =
    blok Rigid [ ( 1, 18 ), ( 2, 11 ) ]


level8 =
    { emptyLevel
        | bloks =
            bloksDict
                [ player4
                , target4
                , fram
                , slid8_1
                , slid8_2
                , slid8_3
                , wall8_1
                , wall8_2
                , wall8_3
                , mini_bloks
                , reset_bloks
                ]
        , name = "zipper slider"
    }


player9 =
    blok Free [ ( 2, 4 ) ]


target9 =
    blok Rigid [ ( 16, 15 ) ]


mini_bloks9 =
    blok Rigid
        [ ( 5, 1 )
        , ( 2, 18 )
        , ( 6, 17 )
        , ( 8, 16 )
        , ( 9, 4 )
        , ( 10, 12 )
        , ( 11, 7 )
        , ( 15, 13 )
        , ( 16, 5 )
        , ( 17, 8 )
        , ( 18, 17 )
        , ( 19, 2 )
        ]


level9 =
    { emptyLevel
        | bloks = bloksDict [ player9, target9, mini_bloks9 ]
        , name = "vortex"
    }


mini_bloks10 =
    blok Rigid
        [ ( 9, 19 )
        , ( 3, 18 )
        , ( 14, 18 )
        , ( 6, 8 )
        , ( 7, 11 )
        ]


level10 =
    { level9
        | bloks = bloksDict [ player9, target9, mini_bloks9, mini_bloks10 ]
        , name = "vortex2"
    }


levels =
    [ level1
    , level2
    , level3
    , level4
    , level6
    , level7
    , level8
    , level9
    , level10
    , finalLevel
    ]
