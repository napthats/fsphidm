module PhiClient

open Server
open PhiMap




type ClientProtocol =
    | Say of string
    | Go of (Direction * bool) //bool is for a withTurn flag

type ServerProtocol =
    | RawMessage of string

type PhiClient =
    abstract Read : unit -> ClientProtocol option
    abstract Write : ServerProtocol -> unit
    abstract Disconnect : unit -> unit
    abstract isConnected : bool

let private parse_direction (dir_string : string) =
    match dir_string.ToLower() with
    | "f" -> Some(RelativeDirection(F))
    | "r" -> Some(RelativeDirection(R))
    | "b" -> Some(RelativeDirection(B))
    | "l" -> Some(RelativeDirection(L))
    | "n" -> Some(AbstractDirection(N))
    | "e" -> Some(AbstractDirection(E))
    | "s" -> Some(AbstractDirection(S))
    | "w" -> Some(AbstractDirection(W))
    | _ -> None

type private InternalPhiClient(client : Client) =
    interface PhiClient with
        member this.Read() = 
            match client.Read() with
            | Some(msg) ->
                match Array.toList(msg.Split()) with
                | ["go"; dir_string] ->
                    match parse_direction(dir_string) with
                    | Some(dir) -> Some(Go(dir, dir_string.ToUpper() = dir_string))
                    | None -> None //TODO: send error messages to the client.
                | _ -> Some(Say(msg))
            | None -> None
        member this.Write(sp: ServerProtocol) = 
            match sp with
                | RawMessage(msg) ->
                    client.Write(msg)
        member this.Disconnect() = client.Disconnect()
        member this.isConnected = client.isConnected
        
let createPhiClient(client) = (new InternalPhiClient(client)) :> PhiClient
