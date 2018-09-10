module DatasetDetails

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.PowerPack.Fetch

type Model =
    { loading: bool
      dataset: Dataset option }

type Msg =
    | LoadDatasetResponse of Result<Dataset, exn>

type InitData =
    | Dataset of Dataset
    | DatasetId of int64

let init (data: InitData) =
    match data with
    | Dataset dataset ->
        let model =
            { loading = false
              dataset = Some dataset }
        model, Cmd.none
    | DatasetId datasetId ->
        let session: Result<Session, string>  = Token.load ()
        match session with
        | Ok session ->
            let model =
                { loading = true
                  dataset = None }

            let cmd = 
                let authorization = sprintf "Bearer %s" session.token
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.GET
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ] ]

                let url = sprintf "/api/datasets/%d" datasetId
                Cmd.ofPromise
                    (fun _ -> fetchAs url Dataset.Decoder defaultProps)
                    ()
                    (Ok >> LoadDatasetResponse)
                    (Error >> LoadDatasetResponse)
            model, cmd
        | Error error ->
            { loading = true; dataset = None }, Cmd.none

let update msg model =
    match msg with
    | LoadDatasetResponse (Ok dataset) ->
        { model with loading = false; dataset = Some dataset}, Cmd.none
    | LoadDatasetResponse (Error err) ->
        { model with loading = false}, Cmd.none

let view model dispatch =
    if model.loading then
        div [] [ str "loading" ]
    else
        match model.dataset with
        | Some dataset ->
            let resources =
                div [] (List.map (fun (resource: Resource) -> p [] [ str (resource.id.ToString()) ]) dataset.resources)
            let tasks =
                div [] (List.map (fun (task: Task) -> p [] [ str (task.id.ToString()) ]) dataset.tasks)
            div []
                [ p [] [ str dataset.title ]
                  div []
                      [ header [ classList [("flex-box", true)]]
                            [ div [classList [("flex-item", true)]] [ str "Resources" ]
                              div [classList [("flex-item", true)]] [ str "Tasks" ]]
                        section []
                            [ div [] [resources]
                              div [] [tasks] ] ] ]
        | None ->
            div [] [ str "not found" ]
