module Main

open Server
open System.Collections.Generic


[<EntryPoint>]
let main _ =
    let client_List = new List<Client>();
    //tentative
    let rnd = System.Random()
    while true do
        Async.Sleep(1000) |> Async.RunSynchronously
        List.map (fun client -> client_List.Add(client)) (getNewClientList()) |> ignore
        for client in client_List do
            match client.Read() with
            //| None -> ()
            | None -> if (rnd.Next(2) = 0) then (client.Disconnect()) //tentative
            | Some msg -> for receiver_client in client_List do receiver_client.Write(msg)
    0