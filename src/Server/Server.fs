open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared

open Microsoft.WindowsAzure.Storage

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = tryGetEnv "public_path" |> Option.defaultValue "../Client/public" |> Path.GetFullPath
let storageAccount = tryGetEnv "STORAGE_CONNECTIONSTRING" |> Option.defaultValue "UseDevelopmentStorage=true" |> CloudStorageAccount.Parse

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

// interesting part starts here

// HttpFunc : HttpContext -> Task<HttpContext option>

type Todo =
    { Id : int
      Description : string
      IsCompleted : bool }

let sampleTodo : Todo =
    { Id = 0
      Description = "implement server side with Saturn"
      IsCompleted = false }

let mutable database = [ sampleTodo ]

let load () : Task<Todo list> =
    task {
        return database
    }

let save (todos: Todo list) =
    task {
        database <- todos
    }

let webApp = router {
    get "/api/todos" (fun next ctx ->
        task {
            let! todos = load()
            return! json todos next ctx
        })
    post "/api/todos" (fun next ctx ->
        task {
            let! todos = ctx.BindModelAsync<Todo list>()
            do! save todos
            return! json "Saved!" next ctx
        })
}

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    service_config configureAzure
    use_gzip
}

run app
