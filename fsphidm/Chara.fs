module Chara

open System.Collections.Generic
open PhiMap

let private SIGHT_SIZE = 7 //tentative


[<AbstractClass>]
type Chara(first_pos, first_adir, _name) as this = // (first_pos: Position, first_dir: ADType) = 
    let mutable pos = first_pos
    let mutable adir = first_adir
    let (name : string) = _name //tentative
    let sight_type = STNormal //tentative?
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
    static member private make_sightchange_info pos adir sight_type =
        let sight = get_sight (pos, adir, SIGHT_SIZE, sight_type) in
        let chara_in_sight = List<CharaInSight>() in
        let sight_wo_pos = List.map (List.map snd) sight in
        List.mapi
            (fun _y line -> 
                List.mapi
                    (fun _x (maybe_pos, _) ->
                        match maybe_pos with
                        | Some(pos) ->
                          if position_dic.ContainsKey(pos)
                          then
                              List.map
                                  (fun (_chara : Chara) ->chara_in_sight.Add({x = _x; y = _y; dir = make_rdir_from_adir adir (_chara.GetDirection()); chara = _chara}))
                                  (List.ofSeq(position_dic.[pos]))
                              |> ignore
                          else ()
                        | None -> ()
                    )
                    line
            )
            sight
        |> ignore
        (sight_wo_pos, List.ofSeq(chara_in_sight))

    //abstract GetAction : unit -> CharaAction option
    abstract DoAction : unit -> unit
    abstract CanEnter : Position -> bool
    abstract Inform : InformType -> unit

    member this.GetName() = name
    member this.GetDirection() = adir
    member this.Look() =
        this.Inform(SightChange(Chara.make_sightchange_info pos adir sight_type))
        
    member this.Say(msg) =
        let chara_list_in_sight =
            List.concat
              (List.map
                (fun (maybe_pos,_) ->
                    match maybe_pos with
                    | None -> []
                    | Some(pos) ->
                      if position_dic.ContainsKey(pos)
                      then List.ofSeq(position_dic.[pos])
                      else []
                )
                (List.concat (get_sight (pos, adir, SIGHT_SIZE, sight_type))))
        in
        List.map (fun (chara : Chara) -> chara.Inform(Said(msg,this))) chara_list_in_sight |> ignore
        
    member this.Walk(walk_dir,with_turn) =
        let new_adir =
            if with_turn
            then match walk_dir with
                 | AD(ad) -> ad
                 | RD(rd) -> make_adir_from_rdir adir rd
            else adir
        let next_pos = get_next_position pos adir walk_dir in
        if this.CanEnter(next_pos)
        then
            Chara.move this pos next_pos
            pos <- next_pos
            adir <- new_adir
            this.Look()
        else
            this.Inform(CannotGo)
            if with_turn then adir <- new_adir
            this.Look()
    member this.Turn(turn_dir) =
        adir <- match turn_dir with | AD(ad) -> ad | RD(rd) -> make_adir_from_rdir adir rd
        this.Look()
        //this.Inform(SightChange(
          // List.map (List.map snd) (get_sight (pos, adir, SIGHT_SIZE))))
    //abstract Say : string -> unit    
   
and CharaAction =
    | Say of string
    | Go of (Direction * bool) //bool is for a withTurn flag
    | Turn of Direction

and [<NoComparison>] CharaInSight = {x : int; y : int; dir : RelativeDirection; chara : Chara}

and [<NoComparison>] InformType =
    | SightChange of (MapChipType list list * CharaInSight list) //tentative: should not be string
    | Said of (string * Chara)
    | CannotGo

//let chara_walk(walk_dir, with_turn) =


