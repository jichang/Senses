module Sign

open Shared.Model
open Saturn

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive
    let createAction ctx = task {
        let! user = Controller.getJson<User> ctx
        let token = Token.generate user
        return! Controller.json ctx { token = token }
    }

let controller = controller {
    create Controller.createAction
}
