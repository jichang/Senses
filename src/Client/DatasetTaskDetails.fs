module DatasetTaskDetails

open Shared.Model
open Elmish
open Fable.Core
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop

type Tool =
    { iconUrl: string
      title: string }

let tools =
    [ { iconUrl = "/images/square.svg"; title = "Square"}
      { iconUrl = "/images/circle.svg"; title = "Circle"}
      { iconUrl = "/images/polygon.svg"; title = "Polygon"}
      { iconUrl = "/images/point.svg"; title = "Point"} ]

type Pagination =
    { currentPage: int64 }

type Model =
    { loading: bool
      datasetId: int64
      taskId: int64
      task: Task option
      resources: ModelCollection<Resource>
      pagination: Pagination
      selectedTool: Tool
      selectedLabel: Label option }

type Msg =
    | LoadTask of Result<Task, exn>
    | LoadResources of Result<ModelCollection<Resource>, exn>
    | ChangeTool of Tool
    | ChangeLabel of Label
    | MouseDown of Point
    | MouseMove of Point
    | MouseUp of Point

let init (datasetId: int64) (taskId: int64) : Model * Cmd<Msg> =
    let model =
        { loading = false
          datasetId = datasetId
          taskId = taskId
          task = None
          resources = { totalCount = 0L; items = [] }
          pagination = { currentPage = 0L }
          selectedTool = tools.[0]
          selectedLabel = None }


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
            let url = sprintf "/api/datasets/%d/tasks/%d/resources" datasetId taskId
            Cmd.ofPromise
                (fun _ -> fetchAs url (ModelCollection<Resource>.Decoder Resource.Decoder) defaultProps)
                ()
                (Ok >> LoadResources)
                (Error >> LoadResources)

        model, Cmd.batch [sliceCmd; resourcesCmd]
    | _ ->
        model, Cmd.none

let update msg model =
    match msg with
    | LoadTask (Ok task) ->
        { model with task = Some task; selectedLabel = Some task.labels.items.[0] }, Cmd.none
    | LoadResources (Ok resources) ->
        { model with resources = resources }, Cmd.none
    | ChangeTool tool ->
        { model with selectedTool = tool }, Cmd.none
    | ChangeLabel label ->
        { model with selectedLabel = Some label }, Cmd.none
    | MouseDown point ->
        printfn "%A" point
        model, Cmd.none
    | MouseUp point ->
        model, Cmd.none
    | MouseMove point ->
        model, Cmd.none

let view (model: Model) dispatch =
    let pageHeader =
        header []
            [ p [] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str " < " ]; str "Dataset" ] ]

    let labelView (label: Label) =
        let isSelected label =
            match model.selectedLabel with
            | Some selectedLabel ->
                 selectedLabel.id = label.id
            | _ ->
                 false

        let classes =
             classList
                [ ("label", true)
                  ("label--current", isSelected label)
                  ("button--block", true)
                  ("button", true) ]

        let style =
            [ Color label.color ]

        let indicatorColor label =
            if isSelected label then label.color else "tranparent"

        button [ Style style; classes; OnClick (fun evt -> dispatch (ChangeLabel label)) ]
            [ span [ ClassName "indicator"; Style [BackgroundColor (indicatorColor label)] ] []
              str label.title ]

    let labelsView =
        match model.task with
        | Some task ->
            List.map labelView task.labels.items
        | None ->
            []

    let toolButton (tool: Tool) =
        let classes =
             classList
                [ ("button--primary", tool.title = model.selectedTool.title )
                  ("tool", true)
                  ("button", true)
                  ("button--outline", true) ]
        button [ classes; OnClick (fun evt -> dispatch (ChangeTool tool)) ]
            [ img [ Src tool.iconUrl ]; span [] [str tool.title ] ]

    let toolbar =
        div [ ClassName "flex__box" ]
            [ div [ ClassName "flex__item" ] (List.map toolButton tools)
              div [] [ button [ ClassName "button button--solid button--primary" ] [ str "Save" ] ] ]

    let imageEditor (resource: Resource) =
        let getPoint (evt: MouseEvent) =
            let boundRect = evt?target?getBoundingClientRect();
            { x = evt.pageX - boundRect?left; y = evt.pageY - boundRect?top }

        let dispatchMouseDown (evt: MouseEvent) =
             MouseDown (getPoint evt)
             |> dispatch

        let dispatchMouseMove (evt: MouseEvent) =
             MouseMove (getPoint evt)
             |> dispatch

        let dispatchMouseUp (evt: MouseEvent) =
             MouseUp (getPoint evt)
             |> dispatch

        svg
            [ OnMouseDown dispatchMouseDown
              OnMouseMove dispatchMouseMove
              OnMouseUp dispatchMouseUp ]
            [ image [ XlinkHref resource.uri ] [] ]

    let videoEditor (resource: Resource) =
        video [] []

    let textEditor (resource: Resource) =
        textarea [] []

    let editor =
        let editor =
            match List.tryItem ((int)model.pagination.currentPage) model.resources.items with
            | Some resource ->
                match resource.``type``.key with
                | ResourceTypeKey.Image ->
                    imageEditor resource
                | ResourceTypeKey.Video ->
                    videoEditor resource
                | ResourceTypeKey.Text ->
                    textEditor resource
            | None ->
                div [ ] [ str "no such resource" ]
        div [ ClassName "flex__box editor" ]
            [ div [ ClassName "flex__item" ] [ div [ ClassName "square" ]  [ div [ ClassName "content" ] [ editor ] ] ]
              div [ ClassName "asidebar" ] labelsView ]

    div []
        [ pageHeader
          div [ ]
              [ toolbar; editor ] ]
