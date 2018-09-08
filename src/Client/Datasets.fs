module Datasets

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared.Model
open Elmish.Browser.Navigation
open Thoth.Json

type Model =
    { loading: bool
      datasets: Dataset list }

type Msg =
    | Init of Result<Dataset list, exn>
    | Choose of Dataset
    | Create

let init () =
    let model =
        { loading = true
          datasets = List.empty }

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
            let decoder = Decode.list Dataset.Decoder
            Cmd.ofPromise
                (fun _ -> fetchAs "/api/datasets" decoder defaultProps)
                ()                
                (Ok >> Init)
                (Error >> Init)
        model, cmd
    | Error _ ->
        model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok datasets) ->
        { model with datasets = datasets; loading = false }, Cmd.none
    | Init (Error exn) ->
        { model with loading = false }, Cmd.none
    | Choose dataset ->
        let datasetUrl = sprintf "/datasets/%i" dataset.id
        model, Navigation.newUrl datasetUrl
    | Create ->
        model, Navigation.newUrl "/datasets/create"


let view (model: Model) (dispatch: Msg -> unit) =
    if model.loading then
        div [] [ str "loading" ]
    else
        if Seq.isEmpty model.datasets then
            div []
                [ div []
                    [ p [] [ str "No datasets yet" ]
                      a [ (OnClick (fun evt -> dispatch Create))] [ str "Create Dataset" ]] ]
        else
            let datasetRow (dataset: Dataset) =
                let datasetId = dataset.id.ToString()
                tr []
                    [ td [] [ str datasetId ]
                      td [] [ str dataset.title ]
                      td [] [ button [ OnClick (fun _ -> dispatch (Choose dataset)) ] [ str "Details" ] ] ]

            let datasetsRow =
                Seq.map datasetRow model.datasets
                |> Seq.toList

            let datasetsTable =
                table []
                    [ tbody [] datasetsRow ]

            let tableHeader =
                header []
                    [ p [] [ str "Datasets" ]
                      button [ OnClick (fun _ -> dispatch Create )] [ str "Create" ] ] 

            div []
                [ div [] [ tableHeader; datasetsTable] ]
