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

type private MapChip = {mutable look: MapChipType}

//tentative setting
let private phi_map =
    Array.map (Array.map (fun chip_type -> {look = chip_type}))
        [|[|MWall; MWall; MWall; MWall; MWall|];
          [|MWall; Space; Space; Space; MWall|];
          [|MWall; Space; Tree;  Space; MWall|];
          [|MWall; Plant; Box;   Plant; MWall|];
          [|MWall; MWall; MWall; MWall; MWall|]
        |]

let get_default_position () = Pos(1,1)