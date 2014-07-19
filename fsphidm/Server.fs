module Server

open System.IO
open System.Net
open System.Net.Sockets
open Microsoft.FSharp.Control


 
type Client = MailboxProcessor<string> * MailboxProcessor<string>



//TODO: handle disconection
let private write_work (client:TcpClient, inbox:MailboxProcessor<string>) =
    use stream = client.GetStream()
    use out = new StreamWriter(stream, AutoFlush = true)
    while true do
        match inbox.Receive() |> Async.RunSynchronously with
        | msg -> out.WriteLine(msg)

let private read_work (client:TcpClient, inbox:MailboxProcessor<string>) =
    use stream = client.GetStream()
    use inp = new StreamReader(stream)
    while not inp.EndOfStream do
        match inp.ReadLine() with
        | line -> inbox.Post(line)
    printfn "closed %A" client.Client.RemoteEndPoint
    client.Close |> ignore

 
let private server = new MailboxProcessor<Client>(fun server_inbox -> async {
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

server.Start();

let getNewClientList () =
    let rec iter () =
        match server.TryReceive(0) |> Async.RunSynchronously with
        | None -> []
        | Some client -> client :: iter ()
    iter()

