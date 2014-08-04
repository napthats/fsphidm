module Chara

open System.Collections.Generic
open PhiMap
open PhiClient


type CharaAction =
    | Say of string
    | Go of (Direction * bool) //bool is for a withTurn flag
    | Turn of Direction

type InformType =
    | SightChange of string //tentative: should not be string
    | CannotGo

[<AbstractClass>]
type Chara(first_pos, first_adir) as this = // (first_pos: Position, first_dir: ADType) = 
    let mutable pos = first_pos
    let mutable adir = first_adir
    do Chara.appear this pos

    static let position_dic = new Dictionary<Position, List<Chara>>()
    //static let internal_position_dic = new Dictionary<Position, List<Chara>>()
    //static member position_dic = internal_position_dic
    //[<DefaultValue>] static val mutable private position_dic : Dictionary<Position, List<Chara>>
    //static do Chara.position_dic <- new Dictionary<Position, List<Chara>>()

    //need to hide
    static member private _add_to_position chara pos=
        if position_dic.ContainsKey(pos)
        then position_dic.[pos].Add(chara)
        else position_dic.Add(pos, new List<Chara>()); position_dic.[pos].Add(chara)
    //need to hide
    //position_dic must contain chara at pos
    static member private _remove_from_position chara pos=
        if position_dic.ContainsKey(pos)
        then if position_dic.[pos].Remove(chara)
             then ()
             else assert(false) //chara doesn't exist at pos
        else assert(false) //no one exists at pos
    //following functions user must keep consistency about chara's position
    static member private appear chara new_pos = Chara._add_to_position chara new_pos
    static member private disappear chara old_pos = Chara._remove_from_position chara old_pos
    static member private move chara old_pos new_pos = Chara._remove_from_position chara old_pos; Chara._add_to_position chara new_pos

    abstract GetAction : unit -> CharaAction option
    abstract CanEnter : Position -> bool
    abstract Inform : InformType -> unit

    member this.Walk(walk_dir,with_turn) =
        let new_adir =
            if with_turn
            then match walk_dir with
                 | AD(ad) -> ad
                 | RD(rd) -> make_adir_from_rdir adir rd
            else adir
        let next_pos = get_next_position pos adir walk_dir in
        if this.CanEnter(next_pos)
        then Chara.move this pos next_pos; pos <- next_pos; adir <- new_adir; this.Inform(SightChange(get_sight_string (pos, adir, 5)))
        else this.Inform(CannotGo); if with_turn then adir <- new_adir; this.Inform(SightChange(get_sight_string (pos, adir, 5)))
    member this.Turn(turn_dir) =
        adir <- match turn_dir with | AD(ad) -> ad | RD(rd) -> make_adir_from_rdir adir rd
        this.Inform(SightChange(get_sight_string (pos, adir, 5)))
    //abstract Say : string -> unit    