module Main

open Server
open System.Collections.Generic


[<EntryPoint>]
let main _ =
    let client_List = new List<Client>();
    while true do
        Async.Sleep(1000) |> Async.RunSynchronously
        List.map (fun client -> client_List.Add(client)) (getNewClientList()) |> ignore
        for client in client_List do
            match (fst client).TryReceive(0) |> Async.RunSynchronously with
            | None -> ()
            | Some msg -> for receiver_client in client_List do (snd receiver_client).Post(msg)
    0