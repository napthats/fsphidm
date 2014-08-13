module PhiClient

open Server
open PhiMap
open Chara


type ClientProtocol =
    | CPSay of string
    | CPGo of (Direction * bool) //bool is for a withTurn flag
    | CPTurn of Direction

type [<NoComparison>] ServerProtocol =
    | RawMessage of string
    | M57 of (MapChipType list list * CharaInSight list)//tentative: it should contains charas and phiobjs

type PhiClient =
    abstract Read : unit -> ClientProtocol option
    abstract Write : ServerProtocol -> unit
    abstract Disconnect : unit -> unit
    abstract isConnected : bool

let private mapchip_to_byte chip =
    match chip with
    | Space  -> ' 'B 
    | PPlate -> 'o'B
    | Plant  -> ':'B
    | Flower -> '+'B
    | Water  -> '_'B
    | Box    -> 'x'B
    | Mist   -> '/'B
    | TGate  -> '>'B
    | Store  -> 's'B
    | Gate   -> '['B
    | Jail   -> 'I'B
    | LBox   -> '%'B
    | Window -> '|'B
    | Tree   -> 'T'B
    | Glass  -> '='B
    | MWall  -> 'H'B
    | LGate  -> '{'B
    | Wall   -> '#'B
    | Rock   -> '@'B
    | Unknown-> '?'B

//tentative: how much computation time?
let private decode_m57_map (m57_map : MapChipType list list) =
    Array.append
        (Array.append
           "#m57 M E        0:"B
           (List.reduce
               (fun a b -> Array.append a b)
               (List.map
                   (List.reduce (fun a b -> Array.append a b))
                   (List.map (List.map (fun (c : MapChipType) -> [|mapchip_to_byte c; byte 128|])) m57_map))))
        (" \n"B)

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

let private decode_relativedirection rdir =
    match rdir with
    | F -> "F"
    | R -> "R"
    | B -> "B"
    | L -> "L"

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
                | ["go"] -> Some(CPGo(RD(F), false))
                | ["turn"; dir_string] ->
                    match parse_direction(dir_string) with
                    | Some(dir) -> Some(CPTurn(dir))
                    | None -> None //TODO: send error messages to the client.
                | _ -> Some(CPSay(msg))
            | None -> None
        member this.Write(sp: ServerProtocol) = 
            match sp with
                | RawMessage(msg) -> client.Write(msg)
                | M57(m57) -> 
                  let chara_in_sight = snd m57 in
                  client.WriteByteArray(decode_m57_map (fst m57))
                  List.map
                      (fun chara -> client.Write(sprintf "#m57 O C0000:%d %d %s %-31s 00 %-15s # 00 \n" chara.x chara.y (decode_relativedirection chara.dir) (chara.chara.GetName()) "human"))
                      chara_in_sight
                  |> ignore
                  client.Write("#m57 .\n")
        member this.Disconnect() = client.Disconnect()
        member this.isConnected = client.isConnected
        
let createPhiClient(client) = (new InternalPhiClient(client)) :> PhiClient
