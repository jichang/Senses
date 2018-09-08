module Server

open System.IO
open Microsoft.Extensions.DependencyInjection
open Saturn
open Giraffe
open Config
open Thoth.Json.Net

open Giraffe.Serialization

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let pageHandler =
    htmlFile (Path.Combine(publicPath, "index.html"))

let apiRouter = router {
    pipe_through (Auth.requireAuthentication JWT)

    forward "/datasets" Datasets.controller
}

let webApp = router {
    get "/datasets" pageHandler

    forward "/api/users" Users.controller
    forward "/api/sessions" Session.controller
    forward "/api" apiRouter
}

let configureSerialization (services:IServiceCollection) =
    let jsonSettings = Newtonsoft.Json.JsonSerializerSettings()
    for converter in Thoth.Json.Net.Converters.converters do
        jsonSettings.Converters.Add(converter)
    services.AddSingleton<IJsonSerializer>(NewtonsoftJsonSerializer jsonSettings)

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    service_config configureSerialization
    use_gzip
    use_jwt_authentication jwtConfig.secret jwtConfig.issuer
}

run app
