module PhiClient

open Server


type ADType = | N | E | S | W

type RDType = | F | R | B | L

type Direction =
    | AbstractDirection of ADType
    | RelativeDirection of RDType

type ClientProtocol =
    | Say of string
    | Go of Direction

type ServerProtocol =
    | RawMessage of string

type PhiClient =
    abstract Read : unit -> ClientProtocol option
    abstract Write : ServerProtocol -> unit
    abstract Disconnect : unit -> unit
    abstract isConnected : bool

let private parse_direction dir =
    match dir with
    | "f" -> Some(RelativeDirection(F))
    | _ -> None

type private InternalPhiClient(client : Client) =
    interface PhiClient with
        member this.Read() = 
            match client.Read() with
            | Some(msg) ->
                match Array.toList(msg.Split()) with
                | ["go"; dir_string] ->
                    match parse_direction(dir_string) with
                    | Some(dir) -> Some(Go(dir))
                    | None -> None //TODO: send error messages to the client.
                | _ -> Some(Say(msg))
            | None -> None
        member this.Write(sp: ServerProtocol) = 
            match sp with
                | RawMessage(msg) ->
                    client.Write(msg)
        member this.Disconnect() = client.Disconnect()
        member this.isConnected = client.isConnected
        
let createPhiClient(cl) = (new InternalPhiClient(cl)) :> PhiClient
