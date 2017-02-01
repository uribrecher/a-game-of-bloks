module LevelBuilder exposing (..)

import Set exposing (Set)
import BaseTypes exposing (..)

blok : Material -> List Loc -> Blok
blok mat struct =
  { material = mat
  , structure = Set.fromList struct
  }

hStick : Material -> Loc -> Int -> Blok
hStick mat (x,y) size =
  List.map2 (,) (List.range x (x + size - 1)) (List.repeat size y)
    |> blok mat

vStick : Material -> Loc -> Int -> Blok
vStick mat (x,y) size =
  List.map2 (,) (List.repeat size x) (List.range y (y + size - 1))
    |> blok mat

cartesianProduct : List a -> List b -> List (a,b)
cartesianProduct xs ys =
  List.concatMap (\x -> List.map (\y -> (x,y)) ys) xs

box : Material -> Loc -> Int -> Blok
box mat (x,y) size =
  cartesianProduct (List.range x (x + size)) (List.range y (y + size))
    |> blok mat

cross : Material -> Loc -> Blok
cross mat (x,y) =
  [(x,y),(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
    |> blok mat

hCross : Material -> Loc -> Blok
hCross mat (x,y) =
  [(x,y),(x-1,y),(x+1,y),(x,y+1)] |> blok mat

corner : Material -> Loc -> Blok
corner mat (x,y) =
  [(x,y), (x+1,y),(x,y+1)] |> blok mat

-- assume all bloks have the same material
union : List Blok -> Blok
union bloks =
  List.foldl
    (\x accum -> blok x.material (Set.union x.structure accum.structure |> Set.toList))
    (blok Rigid []) bloks

frame : Material -> Loc -> Loc -> Blok
frame mat (left,top) (width, height) =
  union [ hStick mat (left, top) width
        , hStick mat (left, top + height - 1) width
        , vStick mat (left, top) height
        , vStick mat (left + width - 1, top) height]

lBlok : Material -> Loc -> Blok
lBlok mat (x,y) =
  [(x,y), (x+1,y), (x,y-1)]
    |> blok mat

oneWay : Loc -> Blok
oneWay (x,y) =
  [(x,y+1),(x,y-1),(x+1,y)]
    |> blok Rigid

twoWay : Loc -> Blok
twoWay (x,y) =
  [(x,y+1), (x+1,y)]
    |> blok Rigid

deadEnd : Loc -> Int -> Int -> Blok
deadEnd (x,y) width height =
  union [
    blok Rigid [(x,y+height)]
  , twoWay (x + width // 2, y)
  , twoWay (x - width // 2, y)
  , twoWay (x + width // 2, y + height)
  , twoWay (x - width // 2, y + height)
  ]

diode : Loc -> Int -> Int -> Blok
diode (x,y) width height =
  union [
    blok Rigid [(x,y+height + 1)]
  , twoWay (x + width // 2, y) |> hFlip
  , twoWay (x - width // 2, y)
  , blok Rigid [(x + width // 2, y + height)]
  , blok Rigid [(x - width // 2, y + height)]
  ]


-- TRANSFORMATIONS -------------------------------------------------------------

hFlipTransformation : Loc -> Loc -> Loc
hFlipTransformation (axis,_) (x,y) =
  (2 * axis - x,y)

vFlipTransformation : Loc -> Loc -> Loc
vFlipTransformation (_,axis) (x,y) =
  (x,2 * axis - y)

rotateTransformation : Loc -> Loc -> Loc
rotateTransformation (ox,oy) (x,y) =
  let
    (dx,dy) = (x - ox, y - oy)
    (rx,ry) = (-dy, dx)
  in
   (rx + ox, ry + ox)

transformBlok : (Loc -> Loc -> Loc) -> Blok -> Blok
transformBlok transf blk =
  let
    origin = List.head (Set.toList blk.structure)
  in
    case origin of
      Just o -> Set.map (transf o) blk.structure |> Set.toList |> blok blk.material
      Nothing -> blok Rigid []

hFlip : Blok -> Blok
hFlip =
  transformBlok hFlipTransformation

vFlip : Blok -> Blok
vFlip =
  transformBlok vFlipTransformation

rotate : Blok -> Blok
rotate =
  transformBlok rotateTransformation
