module Client

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

open Fable.Helpers.React

open Shared.Model
open Router

type Model =
    { session: Result<Session, string>
      page: Page
      user: User option
      sign: Sign.Model Option
      home: Home.Model option
      datasets: Datasets.Model option
      datasetCreate: DatasetCreate.Model option
      datasetDetails: DatasetDetails.Model option }

type Msg =
    | Sign of Sign.Msg
    | Home of Home.Msg
    | Datasets of Datasets.Msg
    | DatasetCreate of DatasetCreate.Msg
    | DatasetDetails of DatasetDetails.Msg

let urlUpdate (result: Page option) (model: Model) : Model * Cmd<Msg> =
    match result with
    | Some page ->
        match page with
        | Page.Home ->
            match model.user with
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
    | None ->
        model, Cmd.none

let init (page: Page option) : Model * Cmd<Msg> =
    let session: Result<Session, string>  = Token.load ()

    let defaultModel =
        { session = session
          user = None
          page = Page.Home
          sign = None
          home = None
          datasets = None
          datasetCreate = None
          datasetDetails = None }

    let model, cmd =
        match session with
        | Ok session ->
            match page with
            | Some page ->
                match page with
                | Page.Home ->
                    let (homeModel, homeCmd) = Home.init session
                    { defaultModel with home = Some homeModel }, Cmd.map Msg.Home homeCmd
                | Page.Datasets ->
                    let (datasetsModel, datasetsCmd) = Datasets.init ()
                    { defaultModel with datasets = Some datasetsModel }, Cmd.map Msg.Datasets datasetsCmd
                | Page.DatasetCreate ->
                    let (datasetCreateModel, datasetCreateCmd) = DatasetCreate.init ()
                    { defaultModel with datasetCreate = Some datasetCreateModel }, Cmd.map Msg.Datasets datasetCreateCmd
                | Page.DatasetDetails datasetId ->
                    let initData = DatasetDetails.InitData.DatasetId datasetId
                    let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.init initData
                    { defaultModel with datasetDetails = Some datasetDetailsModel }, Cmd.map Msg.DatasetDetails datasetDetailsCmd
            | None ->
                defaultModel, Cmd.none
        | Error error ->
            let (signModel, signCmd) = Sign.init ()
            { defaultModel with sign = Some signModel }, Cmd.map Msg.Sign signCmd
 
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
    | Sign signMsg ->
        let (signModel, signCmd) = Sign.update signMsg model.sign.Value
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

let view (model : Model) (dispatch : Msg -> unit) =
    let pageView =
        match model.session with
        | Ok _ ->
            match model.page with
            | Page.Home ->
                Home.view model.home.Value (dispatch << Msg.Home)
            | Page.Datasets ->
                Datasets.view model.datasets.Value (dispatch << Msg.Datasets)
            | Page.DatasetCreate ->
                DatasetCreate.view model.datasetCreate.Value (dispatch << Msg.DatasetCreate)
            | Page.DatasetDetails datasetId ->
                DatasetDetails.view model.datasetDetails.Value (dispatch << Msg.DatasetDetails)
        | Error _ ->
            match model.sign with
            | Some signModel ->
                div [] [ Sign.view signModel (dispatch << Msg.Sign) ]
            | None ->
                div [] [ str "Refresh Page" ]

    div []
        [ pageView ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif


let urlParser: Parser<Page option> = parsePath pageParser

Program.mkProgram init update view
|> Program.toNavigable urlParser urlUpdate
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.runWith ()
