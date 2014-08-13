module Pc

open Chara
open PhiMap
open PhiClient
//type PhiClient = PhiClient.PhiClient

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
