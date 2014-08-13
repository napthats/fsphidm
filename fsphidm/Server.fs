module Server

open System.IO
open System.Net
open System.Net.Sockets
open Microsoft.FSharp.Control


type Client =
    abstract Read : unit -> option<string>
    abstract Write : string -> unit
    abstract WriteByteArray : byte [] -> unit
    abstract Disconnect : unit -> unit
    abstract isConnected : bool

type private WriteProtocol =
    | Write of string
    | WriteByteArray of byte []
    | Close

type private ReadProtocol =
    | Read of string
    | Closed

type private ReadWriteMailbox = MailboxProcessor<ReadProtocol> * MailboxProcessor<WriteProtocol>

type private InternalClient(mbox : ReadWriteMailbox) =
    let mutable is_connected = true
    interface Client with
        member this.Read() =
            match (fst mbox).TryReceive(0) |> Async.RunSynchronously with
            | Some(Read(msg)) -> Some(msg)
            | Some(Closed) -> is_connected <- false; None
            | None -> None
        member this.Write(msg) = (snd mbox).Post(Write(msg))
        member this.WriteByteArray(byte_array) = (snd mbox).Post(WriteByteArray(byte_array))
        member this.Disconnect() = (snd mbox).Post(Close)
        member this.isConnected = is_connected
    
let private write_work (client:TcpClient, inbox:MailboxProcessor<WriteProtocol>) =
    use stream = client.GetStream()
    use out = new StreamWriter(stream, AutoFlush = true)
    let cont = ref true
    try
        while !cont do
            match inbox.Receive() |> Async.RunSynchronously with
            | Close -> cont := false
            | Write(msg) -> out.WriteLine(msg)
            | WriteByteArray(msg) -> stream.Write(msg, 0, msg.Length)
    with
        _ -> ()
    client.Close() //Is it OK to Close twice?

let private read_work (client:TcpClient, inbox:MailboxProcessor<ReadProtocol>, mailbox_write:MailboxProcessor<WriteProtocol>) =
    use stream = client.GetStream()
    use inp = new StreamReader(stream)
    try
        while not inp.EndOfStream do
            match inp.ReadLine() with
            | line -> inbox.Post(Read(line))
    with
        _ -> ()
    mailbox_write.Post(Close)
    inbox.Post(Closed)
    client.Close()

 
let private server = new MailboxProcessor<ReadWriteMailbox>(fun server_inbox -> async {
    let socket = new TcpListener(IPAddress.Loopback, 12321)
    do socket.Start()
    while true do
        let client = socket.AcceptTcpClient()
        let mailbox_write = new MailboxProcessor<WriteProtocol>(fun inbox ->
            async {write_work(client, inbox)}
        )
        let mailbox_read = new MailboxProcessor<ReadProtocol>(fun inbox ->
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

