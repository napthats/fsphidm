module Pc

open Chara
open PhiMap
open PhiClient
//type PhiClient = PhiClient.PhiClient

//Constructor should be private?
type Pc(phi_client : PhiClient, first_pos : Position, first_dir: AbsoluteDirection) =
    inherit Chara(first_pos, first_dir)
     
    override this.GetAction () =
        match phi_client.Read() with
        | Some(CPSay(msg)) -> Some(Say(msg))
        | Some(CPGo(dir, withTurn)) -> Some(Go(dir, withTurn))
        | Some(CPTurn(dir)) -> Some(Turn(dir))
        | None -> None
    override this.CanEnter pos = can_enter pos ETWalk
    override this.Inform inform_type =
        match inform_type with
        | SightChange(sight_string) -> this.Send(RawMessage(sight_string))
        | CannotGo -> this.Send(RawMessage("Cannot go.")) //tentative message
    //tentative
    member this.Send msg = phi_client.Write msg
    //tentative
    member this.isConnected = phi_client.isConnected
