module Home

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared.Model
open Fable.PowerPack.Fetch

type Model =
    { ready: bool }

type Msg =
    | Init

let init (session: Session) =
    { ready = false }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init -> model, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    div [] [ str "Not implemented" ]
