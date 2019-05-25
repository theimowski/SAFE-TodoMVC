open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open Thoth.Json.Net

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

module Azure =
    let blobClient = storageAccount.CreateCloudBlobClient()
    let containerName = "todo-mvc-container"
    let blobRef = blobClient.GetContainerReference containerName
    let leaseTime = TimeSpan.FromSeconds 15.

    let blob =
        task {
            let! _ = blobRef.CreateIfNotExistsAsync ()
            let file = blobRef.GetBlockBlobReference "todos.json"
            let! exists = file.ExistsAsync()
            if not exists then do! file.UploadTextAsync "[]"
            return file
        }
        |> Async.AwaitTask
        |> Async.RunSynchronously

    let getTextFromBlob () =
        blob.DownloadTextAsync ()

    let saveTextToBlob text =
        task {
            let! lease = blob.AcquireLeaseAsync(Nullable.op_Implicit leaseTime, null)
            let condition = AccessCondition.GenerateLeaseCondition lease
            do! blob.UploadTextAsync(text, condition, null, null)
            do! blob.ReleaseLeaseAsync condition
            return ()
        }

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
        let! raw = Azure.getTextFromBlob()
        return Decode.Auto.unsafeFromString raw
    }

let saveEntries (entries) : Task<unit> =
    task {
        let raw = Encode.Auto.toString (0, entries)
        do! Azure.saveTextToBlob raw
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
