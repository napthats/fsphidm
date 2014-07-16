open System.IO
open System.Net
open System.Net.Sockets
open Microsoft.FSharp.Control
open System.Collections.Generic
 
//TODO: handle disconection
let write_work (client:TcpClient, inbox:MailboxProcessor<string>) =
    use stream = client.GetStream()
    use out = new StreamWriter(stream, AutoFlush = true)
    while true do
        match inbox.Receive() |> Async.RunSynchronously with
        | msg -> out.WriteLine(msg)

let read_work (client:TcpClient, inbox:MailboxProcessor<string>) =
    printfn "hi"
    use stream = client.GetStream()
    use inp = new StreamReader(stream)
    while not inp.EndOfStream do
        match inp.ReadLine() with
        | line -> inbox.Post(line)
    printfn "closed %A" client.Client.RemoteEndPoint
    client.Close |> ignore

type Client = MailboxProcessor<string> * MailboxProcessor<string>
 
let server = new MailboxProcessor<Client>(fun server_inbox -> async {
    let socket = new TcpListener(IPAddress.Loopback, 12321)
    do socket.Start()
    while true do
        let client = socket.AcceptTcpClient()
        let mailbox_read = new MailboxProcessor<string>(fun inbox ->
            async {read_work(client, inbox)}
        ) 
        let mailbox_write = new MailboxProcessor<string>(fun inbox ->
            async {write_work(client, inbox)}
        )
        mailbox_read.Start()
        mailbox_write.Start()
        server_inbox.Post(mailbox_read, mailbox_write)
})

[<EntryPoint>]
let main _ =
    let client_List = new List<Client>();
    server.Start()
    while true do
        match server.TryReceive(1000) |> Async.RunSynchronously with
        | None -> ()
        | Some client -> client_List.Add(client)
        for client in client_List do
            match (fst client).TryReceive(0) |> Async.RunSynchronously with
            | None -> ()
            | Some msg -> for receiver_client in client_List do (snd receiver_client).Post(msg)
    0