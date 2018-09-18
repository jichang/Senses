module Home

open Elmish
open Fable.Import.React
open Fable.Helpers.React
open Fable.Helpers.React.Props

open Thoth.Json
open Shared.Model
open Fable.PowerPack.Fetch

type TabKey =
    | Datasets
    | Tasks
    | Labels

type TabState<'t> =
    { loading: bool
      collection: ModelCollection<'t> }

type Model =
    { session: Session
      datasetsTab: TabState<Dataset>
      tasksTab: TabState<Task>
      labelsTab: TabState<Label>
      activeTabKey: TabKey }

type Msg =
    | DatasetsInit of Result<ModelCollection<Dataset>, exn>
    | TasksInit of Result<ModelCollection<Task>, exn>
    | LabelsInit of Result<ModelCollection<Label>, exn>
    | ChangeTab of TabKey

let init (session: Session) =
    let model =
        { session = session
          datasetsTab =
              { loading = true
                collection = { totalCount = 0L; items = [] } }
          tasksTab =
              { loading = false
                collection = { totalCount = 0L; items = [] } }
          labelsTab =
              { loading = false
                collection = { totalCount = 0L; items = [] } }
          activeTabKey = TabKey.Datasets }

    let cmd =
        let authorization = sprintf "Bearer %s" session.token
        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json"
                    Authorization authorization ]]

        let decoder = ModelCollection.Decoder Dataset.Decoder

        Cmd.ofPromise
            (fun _ -> fetchAs "/api/datasets" decoder defaultProps)
            ()
            (Ok >> DatasetsInit)
            (Error >> DatasetsInit)

    model, cmd

let update (msg: Msg) (model: Model) =
    match msg with
    | DatasetsInit  (Ok datasets) ->
        let datasetsTab = { model.datasetsTab with collection = datasets; loading = false }
        { model with datasetsTab = datasetsTab }, Cmd.none
    | DatasetsInit  (Error error) ->
        let datasetsTab = { model.datasetsTab with loading = false }
        { model with datasetsTab = datasetsTab }, Cmd.none
    | TasksInit  (Ok tasks) ->
        let tasksTab = { model.tasksTab with collection = tasks; loading = false }
        { model with tasksTab = tasksTab }, Cmd.none
    | TasksInit  (Error error) ->
        let datasetsTab = { model.datasetsTab with loading = false }
        { model with datasetsTab = datasetsTab }, Cmd.none
    | LabelsInit  (Ok labels) ->
        let labelsTab = { model.labelsTab with collection = labels; loading = false }
        { model with labelsTab = labelsTab }, Cmd.none
    | LabelsInit  (Error error) ->
        let labelsTab = { model.labelsTab with loading = false }
        { model with labelsTab = labelsTab }, Cmd.none
    | ChangeTab tabKey ->
        { model with activeTabKey = tabKey }, Cmd.none

let view (model: Model) (dispatch: Msg -> unit) =
    let tableRows =
        match model.activeTabKey with
        | TabKey.Datasets ->
            let createRow (dataset: Dataset) =
                tr []
                    [ td [] [ p [] [ str (dataset.id.ToString()) ] ]
                      td [] [ p [] [ str dataset.title ] ]
                      td [] [ p [] [ str (dataset.status.ToString()) ] ] ]

            if model.datasetsTab.loading then
                [ tr [] [ td [] [ p [] [ str "loading" ] ] ] ]
            else
                List.map createRow model.datasetsTab.collection.items
        | TabKey.Tasks ->
            if model.tasksTab.loading then
                [ tr [] [ td [] [ p [] [ str "loading" ] ] ] ]
            else
                List.map (fun task -> tr [] [] ) model.tasksTab.collection.items
        | TabKey.Labels ->
            if model.labelsTab.loading then
                [ tr [] [ td [] [ p [] [ str "loading" ] ] ] ]
            else
                List.map (fun task -> tr [] [] ) model.labelsTab.collection.items

    let tableHead =
        match model.activeTabKey with
        | TabKey.Datasets ->
            thead [] [
                tr [] [
                    td [] [str "Id"]
                    td [] [str "Title"]
                    td [] [str "Status"] ] ]
        | TabKey.Tasks ->
            thead [] [
                tr [] [
                    td [] [str "Id"]
                    td [] [str "Type"]
                    td [] [str "Status"] ] ]
        | TabKey.Labels ->
            thead [] [
                tr [] [
                    td [] [str "Id"]
                    td [] [str "Title"]
                    td [] [str "Color"]
                    td [] [str "Status"] ] ]

    div [] [
        header [] [
            nav [] [
                ul [] [
                    li [ OnClick (fun _ -> dispatch (ChangeTab TabKey.Datasets)) ] [ str "Datasets" ]
                    li [ OnClick (fun _ -> dispatch (ChangeTab TabKey.Tasks))] [ str "Tasks" ]
                    li [ OnClick (fun _ -> dispatch (ChangeTab TabKey.Labels))] [ str "Labels" ] ]]]
        section [] [
            table [] [
                tableHead
                tbody [] tableRows ] ] ]
