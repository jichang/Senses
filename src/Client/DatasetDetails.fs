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
                    tbody [] [tr [] [ td [ classList [("text--placeholder", true)]; ColSpan 2 ] [str "No tasks" ]]]
                else
                    let rows =
                        List.map (fun (task: Task) -> tr [] [ td [] [str (task.id.ToString())] ]) dataset.tasks.items
                    tbody [] rows

            let slices =
                if List.isEmpty dataset.slices.items then
                    tbody [] [tr [ ] [ td [classList [("text--placeholder", true)]; ColSpan 2 ] [str "No slices" ]]]
                else
                    let row (datasetSlice: DatasetSlice) =
                        tr []
                            [ td [] [ str (datasetSlice.id.ToString()) ]
                              td [] [ str datasetSlice.title ]
                              td [ ClassName "text--right" ] [ a [ Href (sprintf "/datasets/%d/slices/%d" dataset.id datasetSlice.id) ] [ str "Details" ] ] ]
                    let rows =
                        List.map row dataset.slices.items
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
                                      th [] [ str "Id" ]
                                      th [] [ str "Title" ] ] ]
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
