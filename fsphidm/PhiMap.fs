module PhiMap

open System.Collections.Generic


type AbsoluteDirection = | N | E | S | W

type RelativeDirection = | F | R | B | L

type Direction =
    | AD of AbsoluteDirection
    | RD of RelativeDirection

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

let make_adir_from_rdir adir rdir =
    match (adir, rdir) with
    | (d, F) -> d
    | (N, R) -> E
    | (E, R) -> S
    | (S, R) -> W
    | (W, R) -> N
    | (N, B) -> S
    | (E, B) -> W
    | (S, B) -> N
    | (W, B) -> E
    | (N, L) -> W
    | (E, L) -> N
    | (S, L) -> E
    | (W, L) -> S

let get_next_position pos current_adir dir =
    let adir_to_next_pos adir =
        match pos with
        | Pos(x,y) ->
          match adir with
          | N -> Pos(x,y-1)
          | E -> Pos(x+1,y)
          | S -> Pos(x,y+1)
          | W -> Pos(x-1,y)
    in
    match dir with
    | AD(adir) -> adir_to_next_pos adir
    | RD(rdir) -> adir_to_next_pos (make_adir_from_rdir current_adir rdir)

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
      (fun a b -> a + "\n\r" + b)
      (List.map
        (List.reduce (fun a b -> a + b))
        (List.map (List.map (fun c -> mapchip_to_string c.look)) (get_sight (pos, adir, size))))) + "\n\r"

type EnterType =
    | ETWalk

let can_enter pos (_:EnterType) =
    match pos with
    | Pos(x,y) ->
      match (get_map_chip (x,y)).look with
      | Space
      | PPlate
      | Plant
      | Flower
      | Water
      | Box
      | Mist
      | TGate
      | Store
      | Gate -> true
      | Jail
      | LBox
      | Window
      | Tree
      | Glass
      | MWall
      | LGate
      | Wall
      | Rock
      | Unknown -> false
      
      