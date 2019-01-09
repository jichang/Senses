module DatasetDetails

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Thoth.Json

type Model =
    { loading: bool
      dataset: Dataset option }

type Msg =
    | LoadDatasetResponse of Result<Dataset, exn>
    | DownloadResults of int64 * int64
    | LoadResults of Result<ResourceLabels list, exn>

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
    | DownloadResults (datasetId, taskId) ->
        let session: Result<Session, string>  = Token.load ()
        match session with
        | Ok session ->
            let cmd = 
                let authorization = sprintf "Bearer %s" session.token
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.GET
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ] ]

                let decoder = Decode.Auto.generateDecoder<ResourceLabels list>()
                let url = sprintf "/api/datasets/%d/tasks/%d/results" datasetId taskId
                Cmd.ofPromise
                    (fun _ -> fetchAs url decoder defaultProps)
                    ()
                    (Ok >> LoadResults)
                    (Error >> LoadResults)
            model, cmd
        | Error error ->
            model, Cmd.none
    | _ ->
        model, Cmd.none

let view model dispatch =
    if model.loading then
        div [] [ str "loading" ]
    else
        match model.dataset with
        | Some dataset ->

            let slices =
                if List.isEmpty dataset.slices.items then
                    tbody [] [tr [ ] [ td [classList [("text--placeholder", true)]; ColSpan 3 ] [str "No slices" ]]]
                else
                    let row (datasetSlice: DatasetSlice) =
                        tr []
                            [ td [] [ str (datasetSlice.id.ToString()) ]
                              td [] [ str datasetSlice.title ]
                              td [ ClassName "text--right" ] [ a [ Href (sprintf "/datasets/%d/slices/%d" dataset.id datasetSlice.id) ] [ str "Details" ] ] ]
                    let rows =
                        List.map row dataset.slices.items
                    tbody [] rows

            let tasks =
                if List.isEmpty dataset.tasks.items then
                    tbody [] [tr [] [ td [ classList [("text--placeholder", true)]; ColSpan 3 ] [str "No tasks" ]]]
                else
                    let row (task: Task) =
                        let typeString =
                            match task.``type``.key with
                            | TaskTypeKey.Label -> "Label"

                        tr []
                            [ td [] [str (task.id.ToString())]
                              td [] [str typeString ]
                              td [ ClassName "text--right" ]
                                 [ a [ Href (sprintf "/datasets/%d/tasks/%d" dataset.id task.id) ] [ str "Details" ]
                                   button [ ClassName "button button--flat"; OnClick (fun evt -> dispatch (DownloadResults (dataset.id, task.id))) ] [ str "Download" ] ] ]

                    let rows =
                        List.map row dataset.tasks.items
                    tbody [] rows

            div []
                [ p [] [ a [ Href "/datasets" ] [ str " < " ]; str dataset.title ]
                  div []
                      [
                        section [] [
                          header
                              [ classList [("flex__box", true)] ]
                              [ p [ classList [("flex__item", true)] ] [ str "Tasks" ]
                                a [ Href (sprintf "/datasets/%d/tasks/create" dataset.id); classList [("button button--primary button--solid", true)]] [str "Create Task"] ]
                          table [classList [("table table--fullwidth", true)] ] [
                              thead [] [
                                  tr [] [
                                      th [ ClassName "text--left" ] [ str "Id" ]
                                      th [ ClassName "text--left" ] [ str "Type" ]
                                      th [ ClassName "text--right" ] [] ] ]
                              tasks]]
                        section [] [
                          header
                              [ classList [("flex__box", true)] ]
                              [ p [ classList [("flex__item", true)] ] [ str "Dataset Slices" ]
                                a [ Href (sprintf "/datasets/%d/slices/create" dataset.id); classList [("button button--primary button--solid", true)]] [str "Create dataset slice"] ]
                          table [ classList [("table table--fullwidth", true)] ] [
                              thead [] [
                                  tr [] [
                                      th [ ClassName "text--left" ] [ str "Id" ]
                                      th [ ClassName "text--left" ] [ str "Title" ]
                                      th [ ClassName "text--right" ] [] ] ]
                              slices] ] ] ]
        | None ->
            div [] [ str "not found" ]
