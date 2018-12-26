module Token

open System
open Saturn
open Config
open Shared.Model

open System.Security.Claims
open Microsoft.IdentityModel.Tokens
open Microsoft.IdentityModel.JsonWebTokens
open Thoth.Json.Net

let generate (user: User) =
  let sub = Encode.toString 0 (User.Encoder user)

  let claims = [|
    Claim(JwtRegisteredClaimNames.Sub, sub);
    Claim(JwtRegisteredClaimNames.Jti, Guid.NewGuid().ToString()) |]
  claims
  |> Auth.generateJWT (jwtConfig.secret, SecurityAlgorithms.HmacSha256) jwtConfig.issuer (DateTime.UtcNow.AddHours(1.0))
