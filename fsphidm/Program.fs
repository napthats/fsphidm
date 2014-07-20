module Main

open Server
open PhiClient
open System.Collections.Generic


[<EntryPoint>]
let main _ =
    let phiclient_List = new List<PhiClient>();
    while true do
        Async.Sleep(100) |> Async.RunSynchronously
        List.map (fun client -> phiclient_List.Add(createPhiClient(client))) (getNewClientList()) |> ignore
        let remove_client_List = new List<PhiClient>();
        for phiclient in phiclient_List do
            if not phiclient.isConnected
            then
                printf "disconnected" 
                remove_client_List.Add(phiclient)
            else
                match phiclient.Read() with
                | None -> ()
                //| None -> if (rnd.Next(2) = 0) then (client.Disconnect()) //tentative
                | Some msg ->
                    for receiver_client in phiclient_List do
                    receiver_client.Write(
                        match msg with
                        | Say(say_msg) -> RawMessage(say_msg)
                        | Go(_) -> RawMessage("go somewhere")
                    )
        phiclient_List.RemoveAll(fun phiclient -> remove_client_List.Contains(phiclient)) |> ignore
    0