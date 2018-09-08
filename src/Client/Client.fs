module Client

open Elmish
open Elmish.React
open Elmish.Browser.UrlParser
open Elmish.Browser.Navigation

open Fable.Helpers.React

open Shared.Model
open Router

type Model =
    { page: Page
      user: User option
      home: Home.Model option
      datasets: Datasets.Model option
      datasetCreate: DatasetCreate.Model option
      datasetDetails: DatasetDetails.Model option }

type Msg =
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
                let homeModel, homeCmd = Home.init ()
                { model with home = Some homeModel; page = page }, Cmd.map Home homeCmd
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
    let defaultModel =
        { page = Page.Home
          user = None
          home = None
          datasets = None
          datasetCreate = None
          datasetDetails = None }

    let model, cmd = 
        match page with
        | Some page ->
            match page with
            | Page.Home ->
                let (homeModel, homeCmd) = Home.init ()
                { defaultModel with home = Some homeModel }, Cmd.map Home homeCmd
            | Page.Datasets ->
                let (datasetsModel, datasetsCmd) = Datasets.init ()
                { defaultModel with datasets = Some datasetsModel }, Cmd.map Datasets datasetsCmd
            | Page.DatasetCreate ->
                let (datasetCreateModel, datasetCreateCmd) = DatasetCreate.init ()
                { defaultModel with datasetCreate = Some datasetCreateModel }, Cmd.map Datasets datasetCreateCmd
            | Page.DatasetDetails datasetId ->
                let initData = DatasetDetails.InitData.DatasetId datasetId
                let (datasetDetailsModel, datasetDetailsCmd) = DatasetDetails.init initData
                { defaultModel with datasetDetails = Some datasetDetailsModel }, Cmd.map DatasetDetails datasetDetailsCmd
        | None ->
            defaultModel, Cmd.none
 
    model, cmd

let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
    match msg with
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
        match model.page with
        | Page.Home ->
            Home.view model.home.Value (dispatch << Msg.Home)
        | Page.Datasets ->
            Datasets.view model.datasets.Value (dispatch << Msg.Datasets)
        | Page.DatasetCreate ->
            DatasetCreate.view model.datasetCreate.Value (dispatch << Msg.DatasetCreate)
        | Page.DatasetDetails datasetId ->
            DatasetDetails.view model.datasetDetails.Value (dispatch << Msg.DatasetDetails)

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
