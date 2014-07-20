module Main

open Server
open System.Collections.Generic


[<EntryPoint>]
let main _ =
    let client_List = new List<Client>();
    while true do
        Async.Sleep(1000) |> Async.RunSynchronously
        List.map (fun client -> client_List.Add(client)) (getNewClientList()) |> ignore
        let remove_client_List = new List<Client>();
        for client in client_List do
            if not client.isConnected
            then
                printf "disconnected" 
                remove_client_List.Add(client)
            else
                match client.Read() with
                | None -> ()
                //| None -> if (rnd.Next(2) = 0) then (client.Disconnect()) //tentative
                | Some msg -> for receiver_client in client_List do receiver_client.Write(msg)
        client_List.RemoveAll(fun client -> remove_client_List.Contains(client)) |> ignore
    0