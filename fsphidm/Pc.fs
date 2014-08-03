module Pc

open Chara
open PhiMap
type PhiClient = PhiClient.PhiClient

//Constructor should be private?
type Pc(phi_client : PhiClient, first_pos : Position, first_dir: AbsoluteDirection) =
    inherit Chara(first_pos, first_dir)
     
    override this.GetAction () =
        match phi_client.Read() with
        | Some(PhiClient.Say(msg)) -> Some(Say(msg))
        | Some(PhiClient.Go(dir, withTurn)) -> Some(Go(dir, withTurn))
        | None -> None
    override this.CanEnter pos = can_enter pos ETWalk
    override this.Inform inform_type = match inform_type with | SightChange(sight_string) -> this.Send(PhiClient.RawMessage(sight_string))
    //tentative
    member this.Send msg = phi_client.Write msg
    //tentative
    member this.isConnected = phi_client.isConnected
