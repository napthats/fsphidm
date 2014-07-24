module Pc

open Chara
type PhiClient = PhiClient.PhiClient


type Pc(phi_client : PhiClient) =
    interface Chara with
        member this.GetAction () =
            match phi_client.Read() with
            | Some(PhiClient.Say(msg)) -> Some(Say(msg))
            | Some(PhiClient.Go(dir, withTurn)) -> Some(Go(dir, withTurn))
            | None -> None
