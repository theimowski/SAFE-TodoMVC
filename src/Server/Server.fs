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

type Entry =
    { Id : int
      Description : string
      IsCompleted : bool }

let mutable database : Entry list =
    [ { Id = 0
        Description = "prepare slides"
        IsCompleted = false } ]

let getEntries() : Task<Entry list> =
    task {
        return database
    }

let saveEntries (entries: Entry list) =
    task {
        database <- entries
    }

let webApp = router {
    get "/api/entries" (fun next ctx ->
        task {
            let! counter = getEntries()
            return! json counter next ctx
        })
    post "/api/entries" (fun next ctx ->
        task {
            let! entries = ctx.BindModelAsync<Entry list>()
            do! saveEntries entries
            return! Successful.OK "Saved successfully!" next ctx
        })
}

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

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
