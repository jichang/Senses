module TaskCreate

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.PowerPack.Fetch
open Thoth.Json
open Elmish.Browser.Navigation

type Model =
    { loading: bool }

type Msg =
    | Init

let init () =
    { loading = false }, Cmd.none

let update msg model =
    match msg with
    | Init -> model, Cmd.none

let view model dispatch =
    div [] [ p [] [ str "Not Implemented" ] ]
