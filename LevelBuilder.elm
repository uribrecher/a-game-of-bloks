module LevelBuilder exposing (..)

import Set exposing (Set)
import BaseTypes exposing (..)

blok : Material -> Set Loc -> Blok
blok mat struct =
  { material = mat
  , structure = struct
  }

hStick : Material -> Loc -> Int -> Blok
hStick mat (x,y) size =
  List.map2 (,) (List.range x (x + size - 1)) (List.repeat size y)
    |> Set.fromList
    |> blok mat

vStick : Material -> Loc -> Int -> Blok
vStick mat (x,y) size =
  List.map2 (,) (List.repeat size x) (List.range y (y + size - 1))
    |> Set.fromList
    |> blok mat


cartesianProduct : List a -> List b -> List (a,b)
cartesianProduct xs ys =
  List.concatMap (\x -> List.map (\y -> (x,y)) ys) xs

box : Material -> Loc -> Int -> Blok
box mat (x,y) size =
  cartesianProduct (List.range x (x + size)) (List.range y (y + size))
    |> Set.fromList
    |> blok mat

cross : Material -> Loc -> Blok
cross mat (x,y) =
  [(x,y),(x+1,y),(x-1,y),(x,y-1),(x,y+1)]
    |> Set.fromList
    |> blok mat

-- assume all bloks have the same material
union : List Blok -> Blok
union bloks =
  List.foldl (\x accum -> blok x.material (Set.union x.structure accum.structure)) (blok Rigid Set.empty) bloks

frame : Material -> Loc -> Loc -> Blok
frame mat (left,top) (width, height) =
  union [ hStick mat (left, top) width
        , hStick mat (left, top + height - 1) width
        , vStick mat (left, top) height
        , vStick mat (left + width - 1, top) height]

lBlok : Material -> Loc -> Blok
lBlok mat (x,y) =
  [(x,y), (x+1,y), (x,y-1)]
    |> Set.fromList
    |> blok mat

rotateTransformation : Loc -> Loc -> Loc
rotateTransformation (ox,oy) (x,y) =
  let
    (dx,dy) = (x - ox, y - oy)
    (rx,ry) = (-dy, dx)
  in
   (rx + ox, ry + ox)

rotate : Int -> Blok -> Blok
rotate rotation blk =
  let
    origin = List.head (Set.toList blk.structure)
  in
    case origin of
      Just x -> blok blk.material (Set.map (rotateTransformation x) blk.structure)
      Nothing -> blok Rigid Set.empty
