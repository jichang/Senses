module DatasetTaskDetails

open Elmish
open Fable.Core
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop
open Shared.Model

type ShapeType =
    | Rectangle
    | Circle
    | Polygon
    | Point

type Tool =
    { shapeType: ShapeType
      iconUrl: string
      title: string }

let tools =
    [ { shapeType = ShapeType.Circle; iconUrl = "/images/circle.svg"; title = "Circle"}
      { shapeType = ShapeType.Rectangle; iconUrl = "/images/square.svg"; title = "Rectangle"}
      { shapeType = ShapeType.Polygon; iconUrl = "/images/polygon.svg"; title = "Polygon"}
      { shapeType = ShapeType.Point; iconUrl = "/images/point.svg"; title = "Point"} ]

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
      selectedLabel: Label option
      points: Point list
      resourceLabels: ResourceLabel list
      marking: bool }

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
          selectedLabel = None
          points = List.empty
          resourceLabels = List.empty
          marking = false }


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

let createCircle (startPoint: Point) (lastPoint: Point) =
    let centerX = (startPoint.x + lastPoint.x) / 2.0
    let centerY = (startPoint.y + lastPoint.y) / 2.0
    let xOffset = abs (startPoint.x - lastPoint.x)
    let yOffset = abs (startPoint.y - lastPoint.y)
    let radius = sqrt (xOffset * xOffset + yOffset * yOffset)
    let center = { x = centerX; y = centerY }
    Shape.Circle { center = center; radius = radius / 2.0}

let createRectangle (startPoint: Point) (lastPoint: Point) =
    let originX = min startPoint.x lastPoint.x
    let originY = min startPoint.y lastPoint.y
    let width = abs (startPoint.x - lastPoint.x)
    let height = abs (startPoint.y - lastPoint.y)
    Shape.Rectangle { origin = { x = originX; y = originY }; width = width; height = height }

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
        match model.selectedTool.shapeType with
        | Circle
        | Rectangle ->
            { model with points = [point]; marking = true}, Cmd.none
        | _ ->
            model, Cmd.none
    | MouseMove point ->
        if model.marking then
            match model.selectedTool.shapeType with
            | Circle
            | Rectangle ->
                let points =
                    List.append model.points [point]
                { model with points = points}, Cmd.none
            | _ ->
                model, Cmd.none
        else
            model, Cmd.none
    | MouseUp point ->
        match model.selectedTool.shapeType with
        | Circle ->
            match model.selectedLabel with
            | Some label ->
                let startPoint = List.head model.points
                let circle = createCircle startPoint point
                let resourceLabel =
                    { label = label
                      shape = circle }
                { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none
            | None ->
                { model with points = List.empty; marking = false }, Cmd.none
        | Rectangle ->
            match model.selectedLabel with
            | Some label ->
                let startPoint = List.head model.points
                let square = createRectangle startPoint point
                let resourceLabel =
                    { label = label
                      shape = square }
                { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none
            | None ->
                { model with points = List.empty; marking = false }, Cmd.none
        | _ ->
            { model with marking = false }, Cmd.none

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

        let dispatchMouseEvent msg (evt: MouseEvent) =
            if evt?currentTarget?nodeName = "svg" then
                getPoint evt
                |> msg
                |> dispatch

        let resourceLabelView (resourceLabel: ResourceLabel) =
            match resourceLabel.shape with
            | Shape.Circle c ->
                circle [ Cx c.center.x; Cy c.center.y; R c.radius; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []
            | Shape.Rectangle s ->
                rect [ X s.origin.x; Y s.origin.y; SVGAttr.Width s.width; SVGAttr.Height s.height; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []
            | _ ->
                div [] []

        let resourceLabelViews =
            List.map resourceLabelView model.resourceLabels

        let imageView = image [ XlinkHref resource.uri ] []

        let currentShape =
            match model.selectedTool.shapeType with
            | ShapeType.Circle ->
                if model.points.Length > 2 then
                    let firstPoint = List.head model.points
                    let lastPoint = List.last model.points
                    let circle = createCircle firstPoint lastPoint

                    match model.selectedLabel with
                    | Some label ->
                        let labelView = resourceLabelView { label = label; shape = circle }
                        [labelView]
                    | _ ->
                        []
                else
                    []
            | ShapeType.Rectangle ->
                if model.points.Length > 2 then
                    let firstPoint = List.head model.points
                    let lastPoint = List.last model.points
                    let square = createRectangle firstPoint lastPoint

                    match model.selectedLabel with
                    | Some label ->
                        let labelView = resourceLabelView { label = label; shape = square }
                        [labelView]
                    | _ ->
                        []
                else
                    []
            | _ ->
                []

        let childViews =
            resourceLabelViews
            |> List.append currentShape
            |> List.append [ imageView ]

        svg
            [ OnMouseDown (dispatchMouseEvent MouseDown)
              OnMouseMove (dispatchMouseEvent MouseMove)
              OnMouseUp (dispatchMouseEvent MouseUp) ]
            childViews

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
