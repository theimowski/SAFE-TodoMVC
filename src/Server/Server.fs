open System
open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open Shared

let sampleTodos =
    [ { Id = Guid.NewGuid()
        Title = "get the application up & running"
        Completed = true }
      { Id = Guid.NewGuid()
        Title = "add new Todo"
        Completed = false } ]

type Msg =
    | Get of AsyncReplyChannel<Todo list>
    | Apply of Event

type Store () =

    let mb = MailboxProcessor.Start(fun mb ->
        let rec loop todos =
            async {
                let! msg = mb.Receive()
                match msg with
                | Get channel ->
                    channel.Reply todos
                    return! loop todos
                | Apply event ->
                    let todos' = Todos.apply event todos
                    return! loop todos'
            }
        loop sampleTodos)

    member __.GetTodos() = mb.PostAndReply Get
    member __.Apply(event) = mb.Post (Apply event)

let store = Store()

let execute (command: Command) =
    let todos = store.GetTodos()
    let event = Todos.handle command todos
    event

let webApp = router {
    get Url.todos (fun next ctx ->
        task {
            let todos = store.GetTodos()
            return! json todos next ctx
        })
    post Url.todos (fun next ctx ->
        task {
            let! addDTO = ctx.BindModelAsync<AddDTO>()
            let event = execute (Add addDTO)
            store.Apply event
            return! json event next ctx
        })
    patchf "/api/todo/%s" (fun id next ctx ->
        task {
            let guid = Guid.Parse id
            let! patchDTO = ctx.BindModelAsync<PatchDTO>()
            let event = execute (Patch (guid, patchDTO))
            store.Apply event
            return! json event next ctx
        })
    deletef "/api/todo/%s" (fun id next ctx ->
        task {
            let guid = Guid.Parse id
            let event = execute (Delete guid)
            store.Apply event
            return! json event next ctx
        })
}

let app = application {
    url "http://0.0.0.0:8085/"
    use_router webApp
    memory_cache
    use_static (Path.GetFullPath "../Client/public")
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
}

run app