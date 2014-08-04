module Main

open Server
open PhiClient
open Pc
open Chara
open System.Collections.Generic


[<EntryPoint>]
let main _ =
    //tentative
    //printf "%s" <| PhiMap.get_sight_string(PhiMap.get_default_position (), PhiMap.E, 5)
    //
    //tentative: it should be chara_List
    //tentative: deal with disappearing at disconnect
    let pc_List = new List<Pc>()
    //let phiclient_List = new List<PhiClient>()
    while true do
        Async.Sleep(100) |> Async.RunSynchronously
        List.map
            (fun client ->
                let pc = new Pc((createPhiClient(client)),(PhiMap.get_default_position ()),PhiMap.N) in
                pc.Send(RawMessage("hi"))
                pc_List.Add(pc))
            (getNewClientList())
            |> ignore
        let remove_pc_List = new List<Pc>();
        for pc in pc_List do
            if not pc.isConnected
            then
                printf "disconnected" 
                remove_pc_List.Add(pc)
            else
                match (pc :> Chara).GetAction() with
                | None -> ()
                //| None -> if (rnd.Next(2) = 0) then (client.Disconnect()) //tentative
                | Some(Say(say_msg)) ->
                    for receiver_pc in pc_List do receiver_pc.Send(RawMessage(say_msg))
                | Some(Go(dir,with_turn)) -> (pc :> Chara).Walk(dir, with_turn)
                | Some(Turn(dir)) -> (pc :> Chara).Turn(dir)
        pc_List.RemoveAll(fun phiclient -> remove_pc_List.Contains(phiclient)) |> ignore
    0