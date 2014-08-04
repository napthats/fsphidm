module PhiClient

open Server
open PhiMap


type ClientProtocol =
    | CPSay of string
    | CPGo of (Direction * bool) //bool is for a withTurn flag
    | CPTurn of Direction

type ServerProtocol =
    | RawMessage of string

type PhiClient =
    abstract Read : unit -> ClientProtocol option
    abstract Write : ServerProtocol -> unit
    abstract Disconnect : unit -> unit
    abstract isConnected : bool

let private parse_direction (dir_string : string) =
    match dir_string with
    | "f" -> Some(RD(F))
    | "r" -> Some(RD(R))
    | "b" -> Some(RD(B))
    | "l" -> Some(RD(L))
    | "n" -> Some(AD(N))
    | "e" -> Some(AD(E))
    | "s" -> Some(AD(S))
    | "w" -> Some(AD(W))
    | _ -> None

type private InternalPhiClient(client : Client) =
    interface PhiClient with
        member this.Read() = 
            match client.Read() with
            | Some(msg) ->
                match Array.toList(msg.Split()) with
                //TODO: handle just "go" for "go f"
                | ["go"; dir_string] ->
                    match parse_direction(dir_string.ToLower()) with
                    | Some(dir) -> Some(CPGo(dir, dir_string.ToUpper() = dir_string))
                    | None -> None //TODO: send error messages to the client.
                | ["turn"; dir_string] ->
                    match parse_direction(dir_string) with
                    | Some(dir) -> Some(CPTurn(dir))
                    | None -> None //TODO: send error messages to the client.
                | _ -> Some(CPSay(msg))
            | None -> None
        member this.Write(sp: ServerProtocol) = 
            match sp with
                | RawMessage(msg) ->
                    client.Write(msg)
        member this.Disconnect() = client.Disconnect()
        member this.isConnected = client.isConnected
        
let createPhiClient(client) = (new InternalPhiClient(client)) :> PhiClient
