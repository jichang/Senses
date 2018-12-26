module Session

open System
open Shared.Model
open Saturn
open System.Security.Claims
open Microsoft.IdentityModel.Tokens
open Microsoft.IdentityModel.JsonWebTokens
open Thoth.Json.Net
open Config
open Microsoft.AspNetCore.Http

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let generateToken (user: User) =
        let sub = Encode.toString 0 (User.Encoder user)

        let claims = [|
            Claim(JwtRegisteredClaimNames.Sub, sub);
            Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
        claims
        |> Auth.generateJWT (jwtConfig.secret, SecurityAlgorithms.HmacSha256) jwtConfig.issuer (DateTime.UtcNow.AddHours(1.0))

    let indexAction (ctx: HttpContext) = task {
        return! Controller.json ctx null
    }

    let createAction ctx = task {
        let! user = Controller.getJson<User> ctx
        let token = generateToken user
        return! Controller.json ctx { token = token }
    }

let controller = controller {
    index Controller.indexAction
    create Controller.createAction
}
