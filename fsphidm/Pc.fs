module Pc

open Chara
open PhiMap
open Server
open PhiClient
open System.Collections.Generic

//Constructor should be private?
type Pc(phi_client : PhiClient, first_pos : Position, first_dir: AbsoluteDirection, _name: string) =
    inherit Chara(first_pos, first_dir, _name)
     
    override this.DoAction() =
        match phi_client.Read() with
        | Some(CPSay(say_msg)) -> (this :> Chara).Say(say_msg)
        | Some(CPGo(dir,with_turn)) -> (this :> Chara).Walk(dir, with_turn)
        | Some(CPTurn(dir)) -> (this :> Chara).Turn(dir) 
        | Some(CPLook) -> (this :> Chara).Look()
        | Some(CPSharp) -> () //tentative
        | None -> ()
    override this.CanEnter pos = can_enter pos ETWalk
    override this.Inform inform_type =
        match inform_type with
        | SightChange(sight) -> this.Send(SPM57(sight))
        | Said(msg, chara) -> this.Send(SPRawMessage(sprintf "%s > %s" (chara.GetName()) msg)) //tentative: messages should be in PhiClient.js
        | CannotGo -> this.Send(SPRawMessage("Cannot go.")) //tentative message
    //tentative
    member this.Send msg = phi_client.Write msg
    //tentative
    member this.isConnected = phi_client.isConnected


let private pc_List = new List<Pc>()
let private pc_num_ref = ref 0

let check_new_pc() =
    List.map
        (fun client ->
            let pc = new Pc((createPhiClient(client)),(PhiMap.get_default_position ()),PhiMap.N,(sprintf "pc%d" !pc_num_ref)) in
            pc.Send(SPRawMessage(sprintf "#name pc%d" !pc_num_ref))
            pc.Send(SPRawMessage("#ex-notice land=a"))
            pc.Send(SPRawMessage("#ex-notice area=a"))
            (pc :> Chara).Look()
            incr pc_num_ref
            pc_List.Add(pc))
        (getNewClientList())
        |> ignore

let act_all_pc() =
    let remove_pc_List = new List<Pc>();
    for pc in pc_List do
        if not pc.isConnected
        then
            printf "disconnected" 
            remove_pc_List.Add(pc)
        else
            (pc :> Chara).DoAction()
    pc_List.RemoveAll(fun phiclient -> remove_pc_List.Contains(phiclient)) |> ignore