module Token

open Fable.PowerPack
open Shared.Model
open Thoth.Json

let private sessionKey = "senses.session"

let save (session: Session) = 
    BrowserLocalStorage.save sessionKey session

let load (): Result<Session, string> =
    BrowserLocalStorage.load Session.Decoder sessionKey

let delete () =
    BrowserLocalStorage.delete sessionKey
