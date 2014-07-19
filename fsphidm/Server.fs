module Server

open System.IO
open System.Net
open System.Net.Sockets
open Microsoft.FSharp.Control



type Client =
    abstract Read : unit -> option<string>
    abstract Write : string -> unit
    abstract Disconnect : unit -> unit

type private ReadWriteMailbox = MailboxProcessor<string> * MailboxProcessor<string option>

type private InternalClient(mbox : ReadWriteMailbox) =
    interface Client with
        member this.Read() = (fst mbox).TryReceive(0) |> Async.RunSynchronously
        member this.Write(msg) = (snd mbox).Post(Some(msg))
        member this.Disconnect() = (snd mbox).Post(None)
    

let private write_work (client:TcpClient, inbox:MailboxProcessor<string option>) =
    use stream = client.GetStream()
    use out = new StreamWriter(stream, AutoFlush = true)
    let cont = ref true
    try
        while !cont do
            match inbox.Receive(0) |> Async.RunSynchronously with
            | None -> cont := false
            | Some(msg) -> out.WriteLine(msg)
    with
        _ -> ()
    client.Close() //Is it OK to Close twice?

let private read_work (client:TcpClient, inbox:MailboxProcessor<string>, mailbox_write:MailboxProcessor<string option>) =
    use stream = client.GetStream()
    use inp = new StreamReader(stream)
    try
        while not inp.EndOfStream do
            match inp.ReadLine() with
            | line -> inbox.Post(line)
    with
        _ -> ()
    mailbox_write.Post(None)
    client.Close()

 
let private server = new MailboxProcessor<ReadWriteMailbox>(fun server_inbox -> async {
    let socket = new TcpListener(IPAddress.Loopback, 12321)
    do socket.Start()
    while true do
        let client = socket.AcceptTcpClient()
        let mailbox_write = new MailboxProcessor<string option>(fun inbox ->
            async {write_work(client, inbox)}
        )
        let mailbox_read = new MailboxProcessor<string>(fun inbox ->
            async {read_work(client, inbox, mailbox_write)}
        ) 
        mailbox_write.Start()
        mailbox_read.Start()
        server_inbox.Post(mailbox_read, mailbox_write)
})

server.Start();

let getNewClientList () =
    let rec iter () =
        match server.TryReceive(0) |> Async.RunSynchronously with
        | None -> []
        | Some client -> (InternalClient(client) :> Client) :: iter ()
    iter ()

