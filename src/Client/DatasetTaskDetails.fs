module DatasetTaskDetails

open Elmish
open Fable.Core
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.Core.JsInterop
open Thoth.Json
open Shared.Model

type ShapeType =
    | Rectangle
    | Circle
    | Polygon
    | Polyline
    | Point

type Tool =
    { shapeType: ShapeType
      iconUrl: string
      title: string }

let tools =
    [ { shapeType = ShapeType.Circle; iconUrl = "/images/circle.svg"; title = "Circle"}
      { shapeType = ShapeType.Rectangle; iconUrl = "/images/square.svg"; title = "Rectangle"}
      { shapeType = ShapeType.Polygon; iconUrl = "/images/polygon.svg"; title = "Polygon"}
      { shapeType = ShapeType.Polyline; iconUrl = "/images/polygon.svg"; title = "Polyline"}
      { shapeType = ShapeType.Point; iconUrl = "/images/point.svg"; title = "Point"} ]

type Pagination =
    { currentPage: int }

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
      marking: bool 
      creating: bool }

type Msg =
    | LoadTask of Result<Task, exn>
    | LoadResources of Result<ModelCollection<Resource>, exn>
    | LoadResourceLabels of Result<ModelCollection<ResourceLabel>, exn>
    | ChangeTool of Tool
    | ChangeLabel of Label
    | GotoNext
    | GotoPrev
    | MouseDown of Point
    | MouseMove of Point
    | MouseUp of Point
    | Save
    | SaveResponse of Result<Response, exn>

let init (datasetId: int64) (taskId: int64) : Model * Cmd<Msg> =
    let model =
        { loading = false
          datasetId = datasetId
          taskId = taskId
          task = None
          resources = { totalCount = 0L; items = [] }
          pagination = { currentPage = 0 }
          selectedTool = tools.[0]
          selectedLabel = None
          points = List.empty
          resourceLabels = List.empty
          marking = false
          creating = false }


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

let caculateDistance (pointA: Point) (pointB: Point) =
    let xOffset = abs (pointA.x - pointB.x)
    let yOffset = abs (pointA.y - pointB.y)
    sqrt (xOffset * xOffset + yOffset * yOffset)

let createCircle (startPoint: Point) (lastPoint: Point) =
    let centerX = (startPoint.x + lastPoint.x) / 2.0
    let centerY = (startPoint.y + lastPoint.y) / 2.0
    let distance = caculateDistance startPoint lastPoint
    let center = { x = centerX; y = centerY }
    Shape.Circle { center = center; radius = distance / 2.0}

let createRectangle (startPoint: Point) (lastPoint: Point) =
    let originX = min startPoint.x lastPoint.x
    let originY = min startPoint.y lastPoint.y
    let width = abs (startPoint.x - lastPoint.x)
    let height = abs (startPoint.y - lastPoint.y)
    Shape.Rectangle { origin = { x = originX; y = originY }; width = width; height = height }

let createPolygon (points: Point list) =
    Shape.Polygon { points = points }

let createPolyline (points: Point list) =
    Shape.Polyline { points = points }

let update msg model =
    match msg with
    | LoadTask (Ok task) ->
        { model with task = Some task; selectedLabel = Some task.labels.items.[0] }, Cmd.none
    | LoadResources (Ok resources) ->
        let cmd =
            match List.length resources.items with
            | 0 ->
                Cmd.none
            | _ ->
                let session: Result<Session, string>  = Token.load ()
                match session with
                | Ok session ->
                    let authorization = sprintf "Bearer %s" session.token
                    let defaultProps =
                        [ RequestProperties.Method HttpMethod.GET
                          requestHeaders
                              [ ContentType "application/json" 
                                Authorization authorization ] ]
                    let resource = resources.items.[0]
                    let url = sprintf "/api/datasets/%d/tasks/%d/resources/%d/labels" model.datasetId model.taskId resource.id
                    let decoder = Decode.Auto.generateDecoder<ModelCollection<ResourceLabel>>()
                    Cmd.ofPromise
                        (fun _ -> fetchAs url decoder defaultProps)
                        ()
                        (Ok >> LoadResourceLabels)
                        (Error >> LoadResourceLabels)
                | Error _ ->
                    Cmd.none

        { model with resources = resources }, cmd
    | LoadResourceLabels (Ok resourceLabels) ->
        { model with resourceLabels = resourceLabels.items }, Cmd.none
    | ChangeTool tool ->
        { model with selectedTool = tool; points = List.empty }, Cmd.none
    | ChangeLabel label ->
        { model with selectedLabel = Some label }, Cmd.none
    | GotoPrev ->
        let newPagination = { model.pagination with currentPage = model.pagination.currentPage - 1}
        let session: Result<Session, string>  = Token.load ()
        let cmd =
            match session with
            | Ok session ->
                let authorization = sprintf "Bearer %s" session.token
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.GET
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ] ]
                let resource = model.resources.items.[newPagination.currentPage]
                let url = sprintf "/api/datasets/%d/tasks/%d/resources/%d/labels" model.datasetId model.taskId resource.id
                let decoder = Decode.Auto.generateDecoder<ModelCollection<ResourceLabel>>()
                Cmd.ofPromise
                    (fun _ -> fetchAs url decoder defaultProps)
                    ()
                    (Ok >> LoadResourceLabels)
                    (Error >> LoadResourceLabels)
            | Error _ ->
                Cmd.none
        { model with pagination = newPagination; resourceLabels = List.empty }, cmd
    | GotoNext ->
        let newPagination = { model.pagination with currentPage = model.pagination.currentPage + 1}
        let session: Result<Session, string>  = Token.load ()
        let cmd =
            match session with
            | Ok session ->
                let authorization = sprintf "Bearer %s" session.token
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.GET
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ] ]
                let resource = model.resources.items.[newPagination.currentPage]
                let url = sprintf "/api/datasets/%d/tasks/%d/resources/%d/labels" model.datasetId model.taskId resource.id
                let decoder = Decode.Auto.generateDecoder<ModelCollection<ResourceLabel>>()
                Cmd.ofPromise
                    (fun _ -> fetchAs url decoder defaultProps)
                    ()
                    (Ok >> LoadResourceLabels)
                    (Error >> LoadResourceLabels)
            | Error _ ->
                Cmd.none
        { model with pagination = newPagination; resourceLabels = List.empty }, cmd
    | MouseDown point ->
        match model.selectedTool.shapeType with
        | Point
        | Circle
        | Rectangle
        | Polyline ->
            { model with points = [point]; marking = true}, Cmd.none
        | Polygon ->
            { model with marking = true }, Cmd.none
    | MouseMove point ->
        if model.marking then
            match model.selectedTool.shapeType with
            | Point
            | Circle
            | Rectangle
            | Polyline ->
                let points =
                    List.append model.points [point]
                { model with points = points}, Cmd.none
            | Polygon ->
                model, Cmd.none
        else
            model, Cmd.none
    | MouseUp point ->
        match model.selectedLabel with
        | Some label ->
            match model.selectedTool.shapeType with
            | Point ->
                let resourceLabel =
                    { id = None
                      label = label
                      shape = Shape.Point point }
                { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none                
            | Circle ->
                let startPoint = List.head model.points
                let circle = createCircle startPoint point
                let resourceLabel =
                    { id = None
                      label = label
                      shape = circle }
                { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none
            | Rectangle ->
                let startPoint = List.head model.points
                let square = createRectangle startPoint point
                let resourceLabel =
                    { id = None
                      label = label
                      shape = square }
                { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none
            | Polygon ->
                match List.tryHead model.points with
                | Some firstPoint ->
                    let distance = caculateDistance firstPoint point
                    if distance < 3.0 then
                        let shape = createPolygon model.points
                        let resourceLabel =
                            { id = None
                              label = label
                              shape = shape }
                        { model with points = List.empty; resourceLabels = List.append model.resourceLabels [resourceLabel]; marking = false }, Cmd.none
                    else
                        { model with points = List.append model.points [point]; marking = false }, Cmd.none
                | None ->
                    { model with points = List.append model.points [point]; marking = false }, Cmd.none
            | Polyline ->
                { model with points = List.append model.points [point]; marking = false }, Cmd.none
        | None ->
            { model with points = List.empty; marking = false }, Cmd.none
    | Save ->
        let session: Result<Session, string>  = Token.load ()
        match session with
        | Ok session ->
            let cmd =
                let createParams: ResourceLabelsCreateParams =
                    { labels = model.resourceLabels }

                let authorization = sprintf "Bearer %s" session.token
                let body = Encode.Auto.toString (0, createParams)
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.POST
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ]
                      RequestProperties.Body <| unbox body ]

                let resource = model.resources.items.[(int)model.pagination.currentPage]
                let url = sprintf "/api/datasets/%d/tasks/%d/resources/%d/labels" model.datasetId model.taskId resource.id
                Cmd.ofPromise
                    (fun _ -> fetch url defaultProps)
                    ()
                    (Ok >> SaveResponse)
                    (Error >> SaveResponse)

            { model with creating = true }, cmd
        | _ ->
            model, Cmd.none
    | SaveResponse (Ok labels) ->
        model, Cmd.none
    | _ ->
        model, Cmd.none

let view (model: Model) dispatch =
    let pageHeader =
        header []
            [ p [] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str " < " ]; str "Dataset" ] ]

    let labelView (label: Label) =
        let isSelected (label: Label) =
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

        let indicatorColor (label: Label) =
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
              div [] [ button [ OnClick (fun evt -> dispatch Save); ClassName "button button--solid button--primary" ] [ str "Save" ] ] ]

    let imageEditor (resource: Resource) =
        let getPoint (evt: MouseEvent) =
            let boundRect = evt?currentTarget?getBoundingClientRect();
            { x = evt.pageX - boundRect?left; y = evt.pageY - boundRect?top }

        let dispatchMouseEvent msg (evt: MouseEvent) =
            if evt?currentTarget?nodeName = "svg" then
                getPoint evt
                |> msg
                |> dispatch

        let resourceLabelView (resourceLabel: ResourceLabel) =
            match resourceLabel.shape with
            | Shape.Point p ->
                circle [ Cx p.x; Cy p.y; R 2; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill resourceLabel.label.color ] []
            | Shape.Circle c ->
                circle [ Cx c.center.x; Cy c.center.y; R c.radius; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []
            | Shape.Rectangle s ->
                rect [ X s.origin.x; Y s.origin.y; SVGAttr.Width s.width; SVGAttr.Height s.height; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []
            | Shape.Polygon p ->
                let points = List.map (fun point -> sprintf "%f,%f " point.x point.y) p.points |> String.concat " "
                polygon [ SVGAttr.Points points; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []
            | Shape.Polyline p ->
                let points = List.map (fun point -> sprintf "%f %f " point.x point.y) p.points |> String.concat " "
                polyline [ SVGAttr.Points points; SVGAttr.Stroke resourceLabel.label.color; SVGAttr.StrokeWidth "1"; SVGAttr.Fill "transparent" ] []

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
                        let labelView = resourceLabelView { id = None; label = label; shape = circle }
                        [labelView]
                    | _ ->
                        []
                else
                    []
            | ShapeType.Rectangle ->
                if model.points.Length > 2 then
                    let firstPoint = List.head model.points
                    let lastPoint = List.last model.points
                    let rectangle = createRectangle firstPoint lastPoint

                    match model.selectedLabel with
                    | Some label ->
                        let labelView = resourceLabelView { id = None; label = label; shape = rectangle }
                        [labelView]
                    | _ ->
                        []
                else
                    []
            | ShapeType.Polygon ->
                if model.points.Length >= 2 then
                    let polyline = createPolyline model.points
                    match model.selectedLabel with
                    | Some label ->
                        let labelView = resourceLabelView { id = None; label = label; shape = polyline }
                        [labelView]
                    | _ ->
                        []
                else
                    []
            | ShapeType.Polyline ->
                if model.points.Length >= 2 then
                    let polyline = createPolyline model.points
                    match model.selectedLabel with
                    | Some label ->
                        let labelView = resourceLabelView { id = None; label = label; shape = polyline }
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
            match List.tryItem (model.pagination.currentPage) model.resources.items with
            | Some resource ->
                printfn "%A" resource
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

    let pagination =
        div []
            [ button [ Disabled (model.pagination.currentPage = 0); OnClick (fun _ -> dispatch GotoPrev); ClassName "button button--outline button--primary"  ]
                [ str "Prev" ]
              button [ OnClick (fun _ -> dispatch GotoNext); ClassName "button button--outline button--primary"  ]
                [ str "Next" ] ]

    div []
        [ pageHeader
          div [ ]
              [ toolbar; editor; pagination ] ]
