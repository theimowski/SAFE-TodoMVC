open System
open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open Shared

let sampleTodos =
    [ { Id = Guid.NewGuid()
        Description = "get the application up & running"
        IsCompleted = true }
      { Id = Guid.NewGuid()
        Description = "add new Todo"
        IsCompleted = false } ]
    |> ResizeArray<_>

let load () =
    task {
        return sampleTodos |> Seq.toList
    }

let webApp = router {
    get Url.todos (fun next ctx ->
        task {
            let! todos = load()
            return! json todos next ctx
        })
    post Url.todos (fun next ctx ->
        task {
            let! todo = ctx.BindModelAsync()
            sampleTodos.Add todo
            return! json todo next ctx
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