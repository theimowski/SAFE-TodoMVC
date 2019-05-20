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

let blobClient = storageAccount.CreateCloudBlobClient()
let containerName = "todo-mvc-container"
let blobRef = blobClient.GetContainerReference containerName

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

let blob =
    task {
        let! _ = blobRef.CreateIfNotExistsAsync ()
        let file = blobRef.GetBlockBlobReference "todos.json"
        let! exists = file.ExistsAsync()
        if not exists then do! file.UploadTextAsync ""
        return file
    }
    |> Async.AwaitTask
    |> Async.RunSynchronously

let getModel () =
    task {
        let! json = blob.DownloadTextAsync()
        return Decode.Auto.fromString<Model> json
    }

let leaseTime = TimeSpan.FromSeconds 15.

let save (model: Model) : Task<Model> =
    task {
        let! lease = blob.AcquireLeaseAsync(Nullable.op_Implicit leaseTime, null)
        let json = Encode.Auto.toString(1, model)
        let condition = AccessCondition.GenerateLeaseCondition lease
        do! blob.UploadTextAsync(json, condition, null, null)
        do! blob.ReleaseLeaseAsync condition
        return model
    }

let webApp = router {
    get "/api/model" (fun next ctx ->
        task {
            let! m = getModel ()
            let model =
                match m with
                | Ok model -> model
                | Error _ ->
                        { entries = []
                          editing = None
                          visibility = "all"
                          field = ""
                          uid = 0 }
            return! json model next ctx
        })
    post "/api/save" (fun next ctx ->
        task {
            let! model = ctx.BindModelAsync<Model>()
            let! saved = save model
            return! json saved next ctx
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
