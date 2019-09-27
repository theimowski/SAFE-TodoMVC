open System
open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open Shared

let store = FileStore.FileStore()

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
    patch Url.todos (fun next ctx ->
        task {
            let! patchDTO = ctx.BindModelAsync<PatchAllDTO>()
            let event = execute (PatchAll patchDTO)
            store.Apply event
            return! json event next ctx
        })
    patchf "/api/todo/%s" (fun id next ctx ->
        task {
            let guid = Guid.Parse id
            let! patchDTO = ctx.BindModelAsync<PatchSingleDTO>()
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
    delete Url.todosCompleted (fun next ctx ->
        task {
            let event = execute DeleteCompleted
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