module DatasetTaskDetails

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

type Tool =
    { iconUrl: string
      title: string }

let tools =
    [ { iconUrl = "/images/square.svg"; title = "Square"}
      { iconUrl = "/images/circle.svg"; title = "Circle"} ]

type Model =
    { loading: bool
      datasetId: int64
      taskId: int64
      task: Task option
      resources: ModelCollection<Resource> }

type Msg =
    | LoadTask of Result<Task, exn>
    | LoadResources of Result<ModelCollection<Resource>, exn>

let init (datasetId: int64) (taskId: int64) : Model * Cmd<Msg> =
    let model =
        { loading = false
          datasetId = datasetId
          taskId = taskId
          task = None
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
            let url = sprintf "/api/datasets/%d/tasks/%d" datasetId taskId
            Cmd.ofPromise
                (fun _ -> fetchAs url Task.Decoder defaultProps)
                ()
                (Ok >> LoadTask)
                (Error >> LoadTask)

        let resourcesCmd =
            let url = sprintf "/api/datasets/%d/slices/%d/resources" datasetId taskId
            Cmd.ofPromise
                (fun _ -> fetchAs url (ModelCollection.Decoder Resource.Decoder) defaultProps)
                ()
                (Ok >> LoadResources)
                (Error >> LoadResources)

        model, Cmd.batch [sliceCmd; resourcesCmd]
    | _ ->
        model, Cmd.none

let update msg model =
    match msg with
    | LoadTask (Ok task) ->
        { model with task = Some task }, Cmd.none
    | LoadResources (Ok resources) ->
        { model with resources = resources }, Cmd.none
    | _ ->
        model, Cmd.none

let view model dispatch =
    let pageHeader =
        header []
            [ p [] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str " < " ]; str "Dataset" ] ]

    let labelView (label: Label) =
        div [] [ str label.title ]

    let labelsView =
        match model.task with
        | Some task ->
            List.map labelView task.labels.items
        | None ->
            []

    let toolButton (tool: Tool) =
        button [ ClassName "tool" ]
            [ img [ Src tool.iconUrl ]; span [] [str tool.title ] ]

    let toolbar =
        div [ ClassName "flex__box" ]
            [ div [ ClassName "flex__item" ] (List.map toolButton tools)
              div [] [] ]

    let editor =
        div [ ClassName "flex__box" ]
            [ div [ ClassName "flex__item" ] [ ]
              div [ ] labelsView ]

    div []
        [ pageHeader
          div [ ]
              [ toolbar; editor ] ]
