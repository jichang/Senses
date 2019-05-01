module DatasetSliceDetails

open Shared.Model
open Elmish
open Fable.React
open Fable.React.Props
open Fetch
open Api

type Model =
    { loading: bool
      datasetId: int64
      datasetSliceId: int64
      datasetSlice: DatasetSlice
      resources: ModelCollection<Resource> }

type Msg =
    | LoadDatasetSlice of Result<DatasetSlice, exn>
    | LoadResources of Result<ModelCollection<Resource>, exn>

let init (datasetId: int64) (datasetSliceId: int64) : Model * Cmd<Msg> =
    let model =
        { loading = false
          datasetId = datasetId
          datasetSliceId = datasetSliceId
          datasetSlice = { id = 0L; title = ""; status = 0 }
          resources = { totalCount = 0L; items = [] } }


    let session: Result<Session, string>  = Token.load ()
    match session with
    | Ok session ->
        let authorization = sprintf "Bearer %s" session.token
        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" 
                    Authorization authorization ] ]

        let sliceCmd =
            let url = sprintf "/api/datasets/%d/slices/%d" datasetId datasetSliceId
            Cmd.OfPromise.either
                (fun _ -> fetchAs url defaultProps DatasetSlice.Decoder)
                ()
                (Ok >> LoadDatasetSlice)
                (Error >> LoadDatasetSlice)

        let resourcesCmd =
            let url = sprintf "/api/datasets/%d/slices/%d/resources" datasetId datasetSliceId
            Cmd.OfPromise.either
                (fun _ -> fetchAs url defaultProps (ModelCollection<Resource>.Decoder Resource.Decoder))
                ()
                (Ok >> LoadResources)
                (Error >> LoadResources)

        model, Cmd.batch [sliceCmd; resourcesCmd]
    | _ ->
        model, Cmd.none

let update msg model =
    match msg with
    | LoadDatasetSlice (Ok datasetSlice) ->
        { model with datasetSlice = datasetSlice }, Cmd.none
    | LoadResources (Ok resources) ->
        { model with resources = resources }, Cmd.none
    | _ ->
        model, Cmd.none

let view model dispatch =
    let pageHeader =
        header []
            [ p [] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str " < " ]; str model.datasetSlice.title ] ]

    let resourceCell (resource: Resource) =
        div [ ClassName "grid__cell square"; Style [ BackgroundImage (sprintf "url(%s)" resource.uri) ] ] []

    let resourceCells =
        List.map resourceCell model.resources.items

    div []
        [ pageHeader
          div [ ClassName "grid" ] resourceCells ]
