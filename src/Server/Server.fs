open System
open System.IO

open FSharp.Control.Tasks.V2
open Giraffe
open Saturn

open Shared

let sampleTodos : Todo list =
    [ { Id = Guid.NewGuid()
        Description = "get the application up & running"
        IsCompleted = true }
      { Id = Guid.NewGuid()
        Description = "add new Todo"
        IsCompleted = false } ]

let load () =
    task {
        return sampleTodos
    }

let webApp = router {
    get "/api/todos" (fun next ctx ->
        task {
            let! todos = load()
            return! json todos next ctx
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