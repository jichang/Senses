module Datasets

open Elmish
open Fable.React
open Fable.React.Props
open Fetch

open Shared.Model
open Elmish.Navigation
open Api

type Model =
    { loading: bool
      totalCount: int64
      datasets: Dataset list }

type Msg =
    | Init of Result<ModelCollection<Dataset>, exn>
    | Create

let init () =
    let model =
        { loading = true
          totalCount = 0L
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
            let decoder =(ModelCollection<Dataset>.Decoder Dataset.Decoder)
            Cmd.OfPromise.either
                (fun _ -> fetchAs "/api/datasets" defaultProps decoder)
                ()                
                (Ok >> Init)
                (Error >> Init)
        model, cmd
    | Error _ ->
        model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok response) ->
        { model with totalCount = response.totalCount; datasets = response.items; loading = false }, Cmd.none
    | Init (Error exn) ->
        { model with loading = false }, Navigation.newUrl "/sign"
    | Create ->
        model, Navigation.newUrl "/datasets/create"


let view (model: Model) (dispatch: Msg -> unit) =
    if model.loading then
        div [] [ str "loading" ]
    else
        let datasetRow (dataset: Dataset) =
            let datasetId = dataset.id.ToString()
            tr []
                [ td [] [ str datasetId ]
                  td [] [ str dataset.title ]
                  td [ classList [("text--right", true)] ] [ a [ Href (sprintf "/datasets/%d" dataset.id) ] [ str "Details" ] ] ]

        let datasetsRow =
            if Seq.isEmpty model.datasets then
                [ tr [] [ td [ ColSpan 3 ] [ p [ classList [("text--placeholder", true)] ] [str "no datasets yet" ] ] ] ]
            else
                Seq.map datasetRow model.datasets
                |> Seq.toList

        let datasetsTable =
            let header =
                thead []
                    [ tr [] 
                        [ th [ classList [("text--left", true)] ] [ str "Id" ]
                          th [ classList [("text--left", true)] ] [ str "Title" ]
                          th [ classList [("text--right", true)] ] [ ] ] ]

            table [ classList [("table table--fullwidth", true)] ]
                [ header
                  tbody [] datasetsRow ]

        let pageHeader =
            header [ classList [("flex__box", true)] ]
                [ p [ classList [("flex__item", true)] ] [ a [ Href "/" ] [ str "  <  " ]; str "Datasets" ]
                  button [ classList [("button--primary button--solid", true)]; OnClick (fun _ -> dispatch Create )] [ str "Create" ] ] 

        div []
            [ pageHeader; datasetsTable]
