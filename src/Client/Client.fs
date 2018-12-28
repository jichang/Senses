module Client

open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Elmish
open Elmish.React
open Elmish.Browser
open Elmish.Browser.Navigation
open Elmish.HMR

open Shared.Model
open Router

type SessionState =
    | Checking
    | Valid
    | Invalid

// TODO: a better approach is save all data in a seperate model like
//
// type Model =
//     { store: { datasets: ...; labels: ...; tasks: ...; ... } }
//
// then when navigation change event happens, instream of use Option to
// cache page model, we can just retrive data from this model.store
// and create a new model
type Model =
    { session: Result<Session, string>
      sessionState: SessionState
      page: Page option
      sign: Sign.Model option
      home: Home.Model option
      datasets: Datasets.Model option
      datasetCreate: DatasetCreate.Model option
      datasetDetails: DatasetDetails.Model option
      datasetSliceCreate: DatasetSliceCreate.Model option
      datasetSliceDetails: DatasetSliceDetails.Model option
      datasetTaskCreate: DatasetTaskCreate.Model option
      labels: Labels.Model option
      labelCreate: LabelCreate.Model option
      tasks: Tasks.Model option }

type Msg =
    | SessionCheck of Result<Response, exn>
    | Sign of Sign.Msg
    | Home of Home.Msg
    | Datasets of Datasets.Msg
    | DatasetCreate of DatasetCreate.Msg
    | DatasetDetails of DatasetDetails.Msg
    | DatasetSliceCreate of DatasetSliceCreate.Msg
    | DatasetSliceDetails of DatasetSliceDetails.Msg
    | DatasetTaskCreate of DatasetTaskCreate.Msg
    | Labels of Labels.Msg
    | LabelCreate of LabelCreate.Msg
    | Tasks of Tasks.Msg

let initPage (page: Page option) (model: Model) : Model * Cmd<Msg> =
    match page with
    | Some (Page.Sign redirectUrl) ->
        let signModel, signCmd = Sign.init redirectUrl
        { model with sign = Some signModel; page = page }, Cmd.map Sign signCmd
    | Some Page.Home ->
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
    | Some Page.Datasets ->
        match model.datasets with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let datasetsModel, datasetsCmd = Datasets.init ()
            { model with datasets = Some datasetsModel; page = page }, Cmd.map Datasets datasetsCmd
    | Some Page.DatasetCreate ->
        match model.datasetCreate with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let datasetCreateModel, datasetCreateCmd = DatasetCreate.init ()
            { model with datasetCreate = Some datasetCreateModel; page = page }, Cmd.map DatasetCreate datasetCreateCmd
    | Some (Page.DatasetDetails datasetId) ->
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
    | Some (Page.DatasetSliceCreate datasetId) ->
        match model.datasetSliceCreate with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let datasetSliceCreateModel, datasetSliceCreateCmd = DatasetSliceCreate.init datasetId
            { model with datasetSliceCreate = Some datasetSliceCreateModel; page = page }, Cmd.map DatasetSliceCreate datasetSliceCreateCmd
    | Some (Page.DatasetSliceDetails (datasetId, datasetSliceId)) ->
        let datasetSliceDetailsModel, datasetSliceDetailsCmd = DatasetSliceDetails.init datasetId datasetSliceId
        { model with page = page; datasetSliceDetails = Some datasetSliceDetailsModel }, Cmd.map DatasetSliceDetails datasetSliceDetailsCmd
    | Some (Page.DatasetTaskCreate datasetId) ->
        match model.datasetTaskCreate with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let datasetTaskCreateModel, datasetTaskCreateCmd = DatasetTaskCreate.init datasetId
            { model with datasetTaskCreate = Some datasetTaskCreateModel; page = page }, Cmd.map DatasetTaskCreate datasetTaskCreateCmd
    | Some Page.Labels ->
        match model.labels with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let labelsModel, labelsCmd = Labels.init ()
            { model with labels = Some labelsModel; page = page }, Cmd.map Labels labelsCmd
    | Some Page.LabelCreate ->
        match model.labelCreate with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let labelCreateModel, labelCreateCmd = LabelCreate.init ()
            { model with labelCreate = Some labelCreateModel; page = page }, Cmd.map LabelCreate labelCreateCmd
    | Some Page.Tasks ->
        match model.tasks with
        | Some _ ->
            { model with page = page }, Cmd.none
        | None ->
            let tasksModel, tasksCmd = Tasks.init ()
            { model with tasks = Some tasksModel; page = page }, Cmd.map Tasks tasksCmd
    | None ->
        model, Navigation.newUrl "/"

let init (page: Page option) : Model * Cmd<Msg> =
    let session: Result<Session, string>  = Token.load ()

    let defaultModel =
        { session = session
          sessionState = SessionState.Checking
          page = page
          sign = None
          home = None
          datasets = None
          datasetCreate = None
          datasetDetails = None
          datasetSliceCreate = None
          datasetSliceDetails = None
          datasetTaskCreate = None
          labels = None
          labelCreate = None
          tasks = None }

    let model, cmd =
        match session with
        | Ok session ->
            let authorization = sprintf "Bearer %s" session.token
            let defaultProps =
                [ RequestProperties.Method HttpMethod.GET
                  requestHeaders
                      [ ContentType "application/json" 
                        Authorization authorization ] ]

            let cmd =
                Cmd.ofPromise
                    (fun _ -> fetch "/api/session" defaultProps)
                    ()
                    (Ok >> SessionCheck)
                    (Error >> SessionCheck)
            defaultModel, cmd
        | Error error ->
            let newUrl = sprintf "/sign?redirectUrl=%s" (Router.currentUrl())
            { defaultModel with sessionState = SessionState.Invalid }, Navigation.newUrl newUrl
 
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | SessionCheck (Ok session) ->
        let newModel, newCmd = initPage model.page model
        { newModel with sessionState = SessionState.Valid }, newCmd
    | SessionCheck (Error error) ->
        let newUrl = sprintf "/sign?redirectUrl=%s" (Router.currentUrl())
        { model with sessionState = SessionState.Invalid }, Navigation.newUrl newUrl
    | Sign signMsg ->
        let (signModel, signCmd) = Sign.update signMsg model.sign.Value
        match signMsg with
        | Sign.Msg.LoginResponse (Ok session) ->
            Token.save session
            { model with sign = Some signModel; session = (Ok session); sessionState = SessionState.Valid }, Cmd.map Sign signCmd
        | Sign.Msg.CreateResponse (Ok session) ->
            Token.save session
            { model with sign = Some signModel; session = (Ok session); sessionState = SessionState.Valid }, Cmd.map Sign signCmd
        | _ ->
            { model with sign = Some signModel }, Cmd.map Sign signCmd
    | Home homeMsg ->
        let (homeModel, homeCmd) = Home.update homeMsg model.home.Value
        { model with home = Some homeModel }, Cmd.map Home homeCmd
    | Datasets datasetsMsg ->
        let (datasetsModel, datasetsCmd) = Datasets.update datasetsMsg model.datasets.Value
        { model with datasets = Some datasetsModel }, Cmd.map Datasets datasetsCmd
    | DatasetCreate datasetCreateMsg ->
        match datasetCreateMsg with
        | DatasetCreate.Msg.CreateResponse (Ok dataset) ->
            let (datasetCreateModel, datasetCreateCmd) = DatasetCreate.update datasetCreateMsg model.datasetCreate.Value
            { model with datasetCreate = Some datasetCreateModel }, Cmd.map DatasetCreate datasetCreateCmd
        | _ ->
            let (datasetCreateModel, datasetCreateCmd) = DatasetCreate.update datasetCreateMsg model.datasetCreate.Value
            { model with datasetCreate = Some datasetCreateModel }, Cmd.map DatasetCreate datasetCreateCmd
    | DatasetDetails datasetDetailsMsg ->
        let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.update datasetDetailsMsg model.datasetDetails.Value
        { model with datasetDetails = Some datasetDetailsModel }, Cmd.map DatasetDetails datasetDetailsCmd
    | DatasetSliceCreate datasetSliceCreateMsg ->
        let (datasetSliceCreateModel, datasetSliceCreateCmd) = DatasetSliceCreate.update datasetSliceCreateMsg model.datasetSliceCreate.Value
        { model with datasetSliceCreate = Some datasetSliceCreateModel }, Cmd.map DatasetSliceCreate datasetSliceCreateCmd
    | DatasetSliceDetails datasetSliceDetailsMsg ->
        let (datasetSliceDetailsModel, datasetSliceDetailsCmd) = DatasetSliceDetails.update datasetSliceDetailsMsg model.datasetSliceDetails.Value
        { model with datasetSliceDetails = Some datasetSliceDetailsModel }, Cmd.map DatasetSliceDetails datasetSliceDetailsCmd
    | DatasetTaskCreate taskCreateMsg ->
        let taskCreateModel, taskCreateCmd = DatasetTaskCreate.update taskCreateMsg model.datasetTaskCreate.Value
        { model with datasetTaskCreate = Some taskCreateModel }, Cmd.map DatasetTaskCreate taskCreateCmd
    | Labels labelsMsg ->
        let (labelsModel, labelsCmd) = Labels.update labelsMsg model.labels.Value
        { model with labels = Some labelsModel }, Cmd.map Labels labelsCmd
    | LabelCreate labelCreateMsg ->
        let (labelCreateModel, labelCreateCmd) = LabelCreate.update labelCreateMsg model.labelCreate.Value
        { model with labelCreate = Some labelCreateModel }, Cmd.map LabelCreate labelCreateCmd
    | Tasks tasksMsg ->
        let (tasksModel, tasksCmd) = Tasks.update tasksMsg model.tasks.Value
        { model with tasks = Some tasksModel }, Cmd.map Tasks tasksCmd

let view (model : Model) (dispatch : Msg -> unit) =
    let pageView =
        match model.sessionState with
        | SessionState.Checking ->
            div [] [ p [] [ str "checking" ] ]
        | SessionState.Valid ->
            match model.session with
            | Ok _ ->
                match model.page with
                | Some (Page.Sign _) ->
                    Sign.view model.sign.Value (dispatch << Msg.Sign)
                | Some Page.Home ->
                    Home.view model.home.Value (dispatch << Msg.Home)
                | Some Page.Datasets ->
                    Datasets.view model.datasets.Value (dispatch << Msg.Datasets)
                | Some Page.DatasetCreate ->
                    DatasetCreate.view model.datasetCreate.Value (dispatch << Msg.DatasetCreate)
                | Some (Page.DatasetDetails datasetId) ->
                    DatasetDetails.view model.datasetDetails.Value (dispatch << Msg.DatasetDetails)
                | Some (Page.DatasetSliceCreate datasetId) ->
                    DatasetSliceCreate.view model.datasetSliceCreate.Value (dispatch << Msg.DatasetSliceCreate)
                | Some (Page.DatasetSliceDetails (datasetId, datasetSliceId)) ->
                    DatasetSliceDetails.view model.datasetSliceDetails.Value (dispatch << Msg.DatasetSliceDetails)
                | Some (Page.DatasetTaskCreate datasetId) ->
                    DatasetTaskCreate.view model.datasetTaskCreate.Value (dispatch << Msg.DatasetTaskCreate)
                | Some Page.Labels ->
                    Labels.view model.labels.Value (dispatch << Msg.Labels)
                | Some Page.LabelCreate ->
                    LabelCreate.view model.labelCreate.Value (dispatch << Msg.LabelCreate)
                | Some Page.Tasks ->
                    Tasks.view model.tasks.Value (dispatch << Msg.Tasks)
            | Error _ ->
                match model.sign with
                | Some signModel ->
                    div [] [ Sign.view signModel (dispatch << Msg.Sign) ]
                | None ->
                    div [] []
        | SessionState.Invalid ->
            Sign.view model.sign.Value (dispatch << Msg.Sign)

    let mainContent =
        Fable.Helpers.React.main [ ClassName "app-main" ] [ pageView ]

    div [ ClassName "app" ] [ mainContent ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


let urlUpdate = initPage

let urlParser: Parser<Page option> = UrlParser.parsePath pageParser

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReact "senses-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.runWith ()
