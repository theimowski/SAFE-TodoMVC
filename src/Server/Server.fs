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

type Entry =
    { Description : string
      Id : int
      IsCompleted : bool }

let sampleEntry : Entry =
    { Description = "prepare slides"
      Id = 1
      IsCompleted = false }

let mutable db = [ sampleEntry ]

let loadEntries () : Task<Entry list> =
    task {
        return db
    }

let saveEntries (entries) : Task<unit> =
    task {
        db <- entries
    }

let webApp = router {
    get "/api/entries" (fun next ctx ->
        task {
            let! entries = loadEntries ()
            return! json entries next ctx
        })
    post "/api/entries" (fun next ctx ->
        task {
            let! entries = ctx.BindModelAsync<Entry list>()
            do! saveEntries entries
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
