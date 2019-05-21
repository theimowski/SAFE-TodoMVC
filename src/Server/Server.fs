open Shared

open System.IO
open FSharp.Control.Tasks.V2
open Giraffe
open Microsoft.Extensions.DependencyInjection
open Microsoft.WindowsAzure.Storage
open Saturn

// boring, environment-variable-based configuration

let tryGetEnv name =
    System.Environment.GetEnvironmentVariable name
    |> Option.ofObj

let publicPath =
    tryGetEnv "public_path"
    |> Option.defaultValue "../Client/public"
    |> Path.GetFullPath

let storageAccount =
    tryGetEnv "STORAGE_CONNECTIONSTRING"
    |> Option.defaultValue "UseDevelopmentStorage=true"
    |> CloudStorageAccount.Parse

let port =
    "SERVER_PORT"
    |> tryGetEnv
    |> Option.map uint16
    |> Option.defaultValue 8085us

let configureAzure (services:IServiceCollection) =
    tryGetEnv "APPINSIGHTS_INSTRUMENTATIONKEY"
    |> Option.map services.AddApplicationInsightsTelemetry
    |> Option.defaultValue services

// interesting part starts here

// HttpFunc : HttpContext -> Task<HttpContext option>

let webApp = router {
    get "/api/init" (fun next ctx ->
        task {
            let counter = { Value = 42 }
            return! json counter next ctx
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
