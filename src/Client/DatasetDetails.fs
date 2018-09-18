module DatasetDetails

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
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
            let tasks =
                if List.isEmpty dataset.tasks.items then
                    tbody [] [tr [ classList [("text--placeholder", true)]] [ td [ ColSpan 2.0 ] [str "No tasks" ]]]
                else
                    let rows =
                        List.map (fun (task: Task) -> tr [] [ td [] [str (task.id.ToString())] ]) dataset.tasks.items
                    tbody [] rows

            let resources =
                if List.isEmpty dataset.resources.items then
                    tbody [] [tr [ classList [("text--placeholder", true)]] [ td [ ColSpan 2.0 ] [str "No resources" ]]]
                else
                    let rows =
                        List.map (fun (resource: Resource) -> tr [] [ td [] [str (resource.id.ToString())] ]) dataset.resources.items
                    tbody [] rows

            div []
                [ p [] [ str dataset.title ]
                  div []
                      [
                        section [] [
                          header [ classList [("flex-box", true)] ] [ p [ classList [("flex-item", true)] ] [str "Tasks"]; button [] [str "Create"] ]
                          table [] [
                              thead [] [
                                  tr [] [
                                      th [] [ str "Id" ]
                                      th [] [ str "Title" ] ] ]
                              tasks]]
                        section [] [
                          header [ classList [("flex-box", true)] ] [ p [ classList [("flex-item", true)] ] [str "Resources"]; button [] [str "Create"] ]
                          table [] [
                              thead [] [
                                  tr [] [
                                      th [] [ str "Id" ]
                                      th [] [ str "Title" ] ] ]
                              resources] ] ] ]
        | None ->
            div [] [ str "not found" ]
