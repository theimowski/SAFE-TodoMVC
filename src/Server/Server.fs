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

type Database () =

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

    member __.Get() = mb.PostAndReply Get
    member __.Apply(event) = mb.Post (Apply event)

let database = Database()

let webApp = router {
    get Url.todos (fun next ctx ->
        task {
            let todos = database.Get()
            return! json todos next ctx
        })
    post Url.todos (fun next ctx ->
        task {
            let todos = database.Get()
            let! addDTO = ctx.BindModelAsync<AddDTO>()
            let event = Todos.handle (Add addDTO) todos
            database.Apply event
            return! json event next ctx
        })
    patchf "/api/todo/%s" (fun id next ctx ->
        task {
            let guid = Guid.Parse id
            let todos = database.Get()
            let! patchDTO = ctx.BindModelAsync<PatchDTO>()
            let event = Todos.handle (Patch (guid, patchDTO)) todos
            database.Apply event
            return! json event next ctx
        })
    deletef "/api/todo/%s" (fun id next ctx ->
        task {
            let guid = Guid.Parse id
            let todos = database.Get()
            let event = Todos.handle (Delete guid) todos
            database.Apply event
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