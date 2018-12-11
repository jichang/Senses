module Client

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation
open Elmish.HMR

open Fable.Helpers.React
open Fable.Helpers.React.Props

open Shared.Model
open Router

type Model =
    { session: Result<Session, string>
      page: Page
      sign: Sign.Model Option
      home: Home.Model option
      datasets: Datasets.Model option
      datasetCreate: DatasetCreate.Model option
      datasetDetails: DatasetDetails.Model option
      labels: Labels.Model option
      labelCreate: LabelCreate.Model option
      tasks: Tasks.Model option
      taskCreate: TaskCreate.Model option }

type Msg =
    | ChangePage of Page
    | Sign of Sign.Msg
    | Home of Home.Msg
    | Datasets of Datasets.Msg
    | DatasetCreate of DatasetCreate.Msg
    | DatasetDetails of DatasetDetails.Msg
    | Labels of Labels.Msg
    | LabelCreate of LabelCreate.Msg
    | Tasks of Tasks.Msg
    | TaskCreate of TaskCreate.Msg

let initPage (result: Page option) (model: Model) =
    match result with
    | Some page ->
        match page with
        | Page.Sign ->
            match model.sign with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let signModel, signCmd = Sign.init ()
                { model with sign = Some signModel; page = page }, Cmd.map Sign signCmd
        | Page.Home ->
            match model.home with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                match model.session with
                | Ok session ->
                    let homeModel, homeCmd = Home.init session
                    { model with home = Some homeModel; page = page }, Cmd.map Home homeCmd
                | Error error ->
                    { model with page = page}, Cmd.none
        | Page.Datasets ->
            match model.datasets with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let datasetsModel, datasetsCmd = Datasets.init ()
                { model with datasets = Some datasetsModel; page = page }, Cmd.map Datasets datasetsCmd
        | Page.DatasetCreate ->
            match model.datasetCreate with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let datasetCreateModel, datasetCreateCmd = DatasetCreate.init ()
                { model with datasetCreate = Some datasetCreateModel; page = page }, Cmd.map DatasetCreate datasetCreateCmd
        | Page.DatasetDetails datasetId ->
            match model.datasetDetails with
            | Some datasetDetails ->
                match datasetDetails.dataset with
                | Some dataset ->
                    if dataset.id = datasetId then
                        { model with page = page}, Cmd.none
                    else
                        let initData =
                            match model.datasets with
                            | Some datasetsModel ->
                                let datasetMatch: Dataset option = List.tryFind (fun dataset -> dataset.id = datasetId) datasetsModel.datasets 
                                match datasetMatch with
                                | Some dataset ->
                                    DatasetDetails.InitData.Dataset dataset
                                | None ->
                                    DatasetDetails.InitData.DatasetId datasetId
                            | None ->
                                DatasetDetails.InitData.DatasetId datasetId
                        let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.init initData
                        { model with datasetDetails = Some datasetDetailsModel; page = page }, Cmd.map DatasetDetails datasetDetailsCmd
                | None ->
                    { model with page = page }, Cmd.none
            | None ->
                let initData =
                    match model.datasets with
                    | Some datasetsModel ->
                        let datasetMatch: Dataset option = List.tryFind (fun dataset -> dataset.id = datasetId) datasetsModel.datasets 
                        match datasetMatch with
                        | Some dataset ->
                            DatasetDetails.InitData.Dataset dataset
                        | None ->
                            DatasetDetails.InitData.DatasetId datasetId
                    | None ->
                        DatasetDetails.InitData.DatasetId datasetId
                let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.init initData
                { model with datasetDetails = Some datasetDetailsModel; page = page }, Cmd.map DatasetDetails datasetDetailsCmd
        | Page.Labels ->
            match model.labels with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let labelsModel, labelsCmd = Labels.init ()
                { model with labels = Some labelsModel; page = page }, Cmd.map Labels labelsCmd
        | Page.LabelCreate ->
            match model.labelCreate with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let labelCreateModel, labelCreateCmd = LabelCreate.init ()
                { model with labelCreate = Some labelCreateModel; page = page }, Cmd.map LabelCreate labelCreateCmd
        | Page.Tasks ->
            match model.tasks with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let tasksModel, tasksCmd = Tasks.init ()
                { model with tasks = Some tasksModel; page = page }, Cmd.map Tasks tasksCmd
        | Page.TaskCreate taskId ->
            match model.taskCreate with
            | Some _ ->
                { model with page = page }, Cmd.none
            | None ->
                let taskCreateModel, taskCreateCmd = TaskCreate.init ()
                { model with taskCreate = Some taskCreateModel; page = page }, Cmd.map TaskCreate taskCreateCmd
    | None ->
        model, Navigation.newUrl "/"

let urlUpdate = initPage

let init (page: Page option) : Model * Cmd<Msg> =
    let session: Result<Session, string>  = Token.load ()

    let defaultModel =
        { session = session
          page = Page.Home
          sign = None
          home = None
          datasets = None
          datasetCreate = None
          datasetDetails = None
          labels = None
          labelCreate = None
          tasks = None
          taskCreate = None }

    let model, cmd =
        match session with
        | Ok session ->
            initPage page defaultModel
        | Error error ->
            match page with
            | Some page ->
                match page with
                | Page.Sign ->
                    let (signModel, signCmd) = Sign.init ()
                    { defaultModel with sign = Some signModel }, Cmd.map Msg.Sign signCmd
                | _ ->
                    defaultModel, Navigation.newUrl "/sign"
            | _ ->
                defaultModel, Navigation.newUrl "/sign"
 
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | ChangePage page ->
        let url =
            match page with
            | Page.Home -> "/"
            | Page.Datasets -> "/datasets"
            | Page.Tasks -> "/tasks"
            | Page.Labels -> "/labels"
            | _ -> "/"
        model, Navigation.newUrl url
    | Sign signMsg ->
        let (signModel, signCmd) = Sign.update signMsg model.sign.Value
        match signMsg with
        | Sign.Msg.LoginResponse (Ok session) ->
            Token.save session
            { model with sign = Some signModel; session = (Ok session) }, Cmd.map Sign signCmd
        | _ ->
            { model with sign = Some signModel }, Cmd.map Sign signCmd
    | Home homeMsg ->
        let (homeModel, homeCmd) = Home.update homeMsg model.home.Value
        { model with home = Some homeModel }, Cmd.map Home homeCmd
    | Datasets datasetsMsg ->
        let (datasetsModel, datasetsCmd) = Datasets.update datasetsMsg model.datasets.Value
        { model with datasets = Some datasetsModel }, Cmd.map Datasets datasetsCmd
    | DatasetCreate datasetCreateMsg ->
        let (datasetCreateModel, datasetCreateCmd) = DatasetCreate.update datasetCreateMsg model.datasetCreate.Value
        { model with datasetCreate = Some datasetCreateModel }, Cmd.map DatasetCreate datasetCreateCmd
    | DatasetDetails datasetDetailsMsg ->
        let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.update datasetDetailsMsg model.datasetDetails.Value
        { model with datasetDetails = Some datasetDetailsModel }, Cmd.map DatasetDetails datasetDetailsCmd
    | Labels labelsMsg ->
        let (labelsModel, labelsCmd) = Labels.update labelsMsg model.labels.Value
        { model with labels = Some labelsModel }, Cmd.map Labels labelsCmd
    | LabelCreate labelCreateMsg ->
        let (labelCreateModel, labelCreateCmd) = LabelCreate.update labelCreateMsg model.labelCreate.Value
        { model with labelCreate = Some labelCreateModel }, Cmd.map LabelCreate labelCreateCmd
    | Tasks tasksMsg ->
        let (tasksModel, tasksCmd) = Tasks.update tasksMsg model.tasks.Value
        { model with tasks = Some tasksModel }, Cmd.map Tasks tasksCmd
    | TaskCreate taskCreateMsg ->
        let taskCreateModel, taskCreateCmd = TaskCreate.update taskCreateMsg model.taskCreate.Value
        { model with taskCreate = Some taskCreateModel }, Cmd.map TaskCreate taskCreateCmd

let view (model : Model) (dispatch : Msg -> unit) =
    let pageView =
        match model.session with
        | Ok _ ->
            match model.page with
            | Page.Sign ->
                Sign.view model.sign.Value (dispatch << Msg.Sign)
            | Page.Home ->
                Home.view model.home.Value (dispatch << Msg.Home)
            | Page.Datasets ->
                Datasets.view model.datasets.Value (dispatch << Msg.Datasets)
            | Page.DatasetCreate ->
                DatasetCreate.view model.datasetCreate.Value (dispatch << Msg.DatasetCreate)
            | Page.DatasetDetails datasetId ->
                DatasetDetails.view model.datasetDetails.Value (dispatch << Msg.DatasetDetails)
            | Page.Labels ->
                Labels.view model.labels.Value (dispatch << Msg.Labels)
            | Page.LabelCreate ->
                LabelCreate.view model.labelCreate.Value (dispatch << Msg.LabelCreate)
            | Page.Tasks ->
                Tasks.view model.tasks.Value (dispatch << Msg.Tasks)
        | Error _ ->
            match model.sign with
            | Some signModel ->
                div [] [ Sign.view signModel (dispatch << Msg.Sign) ]
            | None ->
                div [] [ str "Refresh Page" ]

    let sideBar =
        aside [ classList [("app-sidebar", true)] ]
            [ nav [ classList [("app-nav", true)] ] [
                ul [ classList [("menu", true)] ] [
                    li [ classList [("menu-item", true)]; OnClick (fun _ -> dispatch (ChangePage Page.Home)) ] [ str "Home" ]
                    li [ classList [("menu-item", true)]; OnClick (fun _ -> dispatch (ChangePage Page.Datasets)) ] [ str "Datasets" ]
                    li [ classList [("menu-item", true)]; OnClick (fun _ -> dispatch (ChangePage Page.Tasks)) ] [ str "Tasks" ]
                    li [ classList [("menu-item", true)]; OnClick (fun _ -> dispatch (ChangePage Page.Labels)) ] [ str "Labels" ] ]] ]

    let mainContent =
        Fable.Helpers.React.main [ classList [("app-main", true) ] ] [ pageView ]

    div [ classList [("app", true) ] ] [ sideBar; mainContent ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


let urlParser: Parser<Page option> = parsePath pageParser

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.runWith ()
