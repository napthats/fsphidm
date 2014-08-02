module PhiMap

open System.Collections.Generic


type ADType = | N | E | S | W

type RDType = | F | R | B | L

type Direction =
    | AbstractDirection of ADType
    | RelativeDirection of RDType

//Map chip appearance
type MapChipType =
    | Space
    | PPlate
    | Plant
    | Flower
    | Water
    | Box
    | Mist
    | TGate
    | Store
    | Gate
    | Jail
    | LBox
    | Window
    | Tree
    | Glass
    | MWall
    | LGate
    | Wall
    | Rock
    | Unknown

type Position = private Pos of int * int

let get_default_position () = Pos(1,1)

type private MapChip = {mutable look: MapChipType}
let private MAP_CHIP_OUTER = {look = Unknown}

//tentative setting
//phi_map must be recangle
let private phi_map =
    Array.map (Array.map (fun chip_type -> {look = chip_type}))
        [|[|MWall; MWall; MWall; MWall; MWall|];
          [|MWall; Space; Space; Space; MWall|];
          [|MWall; Space; Tree;  Space; MWall|];
          [|MWall; Plant; Box;   Plant; MWall|];
          [|MWall; MWall; MWall; MWall; MWall|]
        |]

let private get_map_chip (x,y) =
    if (x<0 || x>=phi_map.[0].Length || y<0 || y>= phi_map.Length)
    then MAP_CHIP_OUTER
    else phi_map.[y].[x]

let rec private list_iterate_n elem iter_func n =
    if n = 0
    then []
    else elem :: list_iterate_n (iter_func elem) iter_func (n-1)

let private get_sub_phigraph ((x,y), adir, size) =
    match adir with
    | N -> List.map (List.map get_map_chip) (list_iterate_n (list_iterate_n (x,y) (fun (x,y) -> (x+1,y)) size) (List.map (fun (x,y) -> (x,y+1))) size)
    | E -> List.map (List.map get_map_chip) (list_iterate_n (list_iterate_n (x+size-1,y) (fun (x,y) -> (x,y+1)) size) (List.map (fun (x,y) -> (x-1,y))) size)
    | S -> List.map (List.map get_map_chip) (list_iterate_n (list_iterate_n (x+size-1,y+size-1) (fun (x,y) -> (x-1,y)) size) (List.map (fun (x,y) -> (x,y-1))) size)
    | W -> List.map (List.map get_map_chip) (list_iterate_n (list_iterate_n (x,y+size-1) (fun (x,y) -> (x,y-1)) size) (List.map (fun (x,y) -> (x+1,y))) size)

//tentative
let private get_sight (pos, adir, size) =
    match pos with
    | Pos(x,y) ->
      let upper_left =
          match adir with
          | N -> (x - size/2, y - size/2 - 1)
          | E -> (x - size/2 + 1, y - size/2)
          | S -> (x - size/2, y - size/2 + 1)
          | W -> (x - size/2 - 1, y - size/2)
      in
      get_sub_phigraph (upper_left, adir, size)

//tentative
let private mapchip_to_string chip =
    match chip with
    | Space  -> "  "
    | PPlate -> "o "
    | Plant  -> "::"
    | Flower -> "+:"
    | Water  -> "__"
    | Box    -> "xx"
    | Mist   -> "//"
    | TGate  -> "><"
    | Store  -> "ss"
    | Gate   -> "[]"
    | Jail   -> "II"
    | LBox   -> "%%"
    | Window -> "||"
    | Tree   -> "TT"
    | Glass  -> "=="
    | MWall  -> "HH"
    | LGate  -> "{}"
    | Wall   -> "##"
    | Rock   -> "@@"
    | Unknown-> "??"

//tentative
let get_sight_string (pos, adir, size) =
    (List.reduce
      (fun a b -> a + "\n" + b)
      (List.map
        (List.reduce (fun a b -> a + b))
        (List.map (List.map (fun c -> mapchip_to_string c.look)) (get_sight (pos, adir, size))))) + "\n"