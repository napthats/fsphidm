module Main

open Pc

[<EntryPoint>]
let main _ =
    while true do
        Async.Sleep(100) |> Async.RunSynchronously
        check_new_pc()
        act_all_pc()
    0