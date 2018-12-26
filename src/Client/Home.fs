module Home

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared.Model
open Fable.PowerPack.Fetch

type Model =
    { ready: bool
      summary: Summary option }

type Msg =
    | Init of Result<Summary, exn>

let init (session: Session) =
    let session: Result<Session, string> = Token.load ()
    match session with
    | Ok session ->
        let authorization = sprintf "Bearer %s" session.token

        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" 
                    Authorization authorization ]]

        let cmd =
            let decoder = Summary.Decoder
            Cmd.ofPromise
                (fun _ -> fetchAs "/api/summary" decoder defaultProps)
                ()                
                (Ok >> Init)
                (Error >> Init)
        { ready = false; summary = None }, cmd
    | Error _ ->
        { ready = false; summary = None }, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok response) ->
      { model with summary = Some response; ready = true }, Cmd.none
    | Init (Error response) ->
      { model with ready = true }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    match model.summary with
    | None ->
        div [] [ str "Not implemented" ]
    | Some summary ->
        let item (title: string) (count: int64) (link: string) =
            div
                [ classList [("card__body", true)] ]
                [ p [] [ str (title + count.ToString()) ]
                  a [ Href link ] [ str "Details" ] ]
        div [ classList [("flex__box", true)] ]
            [ div [classList [("flex__item text--center card card--padding", true)]] [ item "Datasets: " summary.datasetsCount "/datasets" ]
              div [classList [("flex__item text--center card card--padding", true)]] [ item "Labels: " summary.labelsCount "/labels" ] ]
