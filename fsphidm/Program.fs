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
    //tentative: for name id
    let pc_num_ref = ref 0
    //let phiclient_List = new List<PhiClient>()
    while true do
        Async.Sleep(100) |> Async.RunSynchronously
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
        let remove_pc_List = new List<Pc>();
        for pc in pc_List do
            if not pc.isConnected
            then
                printf "disconnected" 
                remove_pc_List.Add(pc)
            else
                (pc :> Chara).DoAction()
        pc_List.RemoveAll(fun phiclient -> remove_pc_List.Contains(phiclient)) |> ignore
    0