module Server

open System.IO
open Microsoft.Extensions.DependencyInjection
open Saturn
open Giraffe
open Config

open Giraffe.Serialization

let publicPath = Path.GetFullPath "../Client/public"
let port = 8085us

let apiRouter = router {
    pipe_through (Auth.requireAuthentication JWT)

    forward "/session" Session.controller
    forward "/summary" Summary.controller
    forward "/labels" Labels.controller
    forward "/datasets" Datasets.controller
    forward "/tasks" Tasks.controller
    forward "/tasktypes" TaskTypes.controller
    forward "/resourcetypes" ResourceTypes.controller
}

let webApp = router {
    forward "/api/signin" Sign.controller
    forward "/api/users" Users.controller
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
