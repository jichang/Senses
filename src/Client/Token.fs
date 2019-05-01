module Token

open Shared.Model
open Thoth.Json
open Browser.WebStorage

let private sessionKey = "senses.session"

let save (session: Session) =
    let json = Encode.toString 0 (session)
    localStorage.setItem (sessionKey, json)
    |> ignore

let load (): Result<Session, string> =
    let json = localStorage.getItem sessionKey
    Decode.fromString Session.Decoder json

let delete () =
    localStorage.removeItem sessionKey
