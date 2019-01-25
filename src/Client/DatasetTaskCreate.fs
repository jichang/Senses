module DatasetTaskCreate

open Shared.Model
open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch
open Fable.Import.React
open Thoth.Json
open Elmish.Browser.Navigation

type Model =
    { loading: bool
      datasetId: int64
      taskTypes: ModelCollection<TaskType>
      labels: ModelCollection<Label>
      slices: ModelCollection<DatasetSlice>
      creating: bool
      choosedTaskType: TaskType option
      choosedLabels: Label list
      choosedSlices: DatasetSlice list }

type Msg =
    | FetchTaskTypes of Result<ModelCollection<TaskType>, exn>
    | FetchLabels of Result<ModelCollection<Label>, exn>
    | FetchSlices of Result<ModelCollection<DatasetSlice>, exn>
    | ChooseTaskType of TaskType
    | ToggleTaskLabel of Label
    | ToggleDatasetSlice of DatasetSlice
    | Submit
    | CreateResponse of Result<Task, exn>

let init (datasetId: int64) =
    let model =
        { loading = true
          datasetId = datasetId
          taskTypes = { totalCount = 0L; items = [] }
          labels = { totalCount = 0L; items = [] }
          slices = { totalCount = 0L; items = [] }
          creating = false
          choosedTaskType = None
          choosedSlices = List.empty
          choosedLabels = List.empty }

    let cmd =
        let session: Result<Session, string> = Token.load ()
        match session with
        | Ok session ->
            let authorization = sprintf "Bearer %s" session.token

            let defaultProps =
                [ RequestProperties.Method HttpMethod.GET
                  requestHeaders
                      [ ContentType "application/json" 
                        Authorization authorization ]]

            let taskTypesCmd =
                let decoder = (ModelCollection<TaskType>.Decoder TaskType.Decoder)
                Cmd.ofPromise
                    (fun _ -> fetchAs "/api/tasktypes" decoder defaultProps)
                    ()                
                    (Ok >> FetchTaskTypes)
                    (Error >> FetchTaskTypes)

            let labelsCmd =
                let decoder = (ModelCollection<Label>.Decoder Label.Decoder)
                Cmd.ofPromise
                    (fun _ -> fetchAs "/api/labels" decoder defaultProps)
                    ()                
                    (Ok >> FetchLabels)
                    (Error >> FetchLabels)

            let slicesCmd =
                let decoder = (ModelCollection<DatasetSlice>.Decoder DatasetSlice.Decoder)
                Cmd.ofPromise
                    (fun _ -> fetchAs (sprintf "/api/datasets/%d/slices" datasetId) decoder defaultProps)
                    ()                
                    (Ok >> FetchSlices)
                    (Error >> FetchSlices)

            Cmd.batch [ taskTypesCmd; labelsCmd; slicesCmd ]
        | Error _ ->
            Cmd.none

    model, cmd

let update msg model =
    match msg with
    | FetchTaskTypes (Ok taskTypes) ->
        { model with taskTypes = taskTypes; choosedTaskType = Some taskTypes.items.[0] }, Cmd.none
    | FetchLabels (Ok labels) ->
        { model with labels = labels }, Cmd.none
    | FetchSlices (Ok slices) ->
        { model with slices = slices }, Cmd.none
    | ChooseTaskType taskType ->
        { model with choosedTaskType = Some taskType }, Cmd.none
    | ToggleTaskLabel label ->
        let isSameLabel (_label: Label) =
            _label.id = label.id

        if List.exists isSameLabel model.choosedLabels then
            { model with choosedLabels = List.filter (not << isSameLabel) model.choosedLabels }, Cmd.none
        else
            { model with choosedLabels = List.append model.choosedLabels [label]  }, Cmd.none
    | ToggleDatasetSlice slice ->
        let isSameSlice (_slice: DatasetSlice) =
            _slice.id = slice.id

        if List.exists isSameSlice model.choosedSlices then
            { model with choosedSlices = List.filter (not << isSameSlice) model.choosedSlices }, Cmd.none
        else
            { model with choosedSlices = List.append model.choosedSlices [slice]  }, Cmd.none
    | Submit ->
        let session: Result<Session, string>  = Token.load ()
        match session, model.choosedTaskType with
        | Ok session, Some taskType ->
            let cmd =
                let createParams: TaskCreateParams =
                    { taskType = taskType
                      datasetSlices = model.choosedSlices
                      labels = model.choosedLabels }

                let authorization = sprintf "Bearer %s" session.token
                let body = Encode.toString 0 (TaskCreateParams.Encoder createParams)
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.POST
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ]
                      RequestProperties.Body <| unbox body ]

                let url = sprintf "/api/datasets/%d/tasks" model.datasetId
                Cmd.ofPromise
                    (fun _ -> fetchAs url Task.Decoder defaultProps)
                    ()
                    (Ok >> CreateResponse)
                    (Error >> CreateResponse)

            { model with creating = true }, cmd
        | _ ->
            model, Cmd.none

    | CreateResponse (Ok task) ->
        let datasetUrl = sprintf "/datasets/%i" model.datasetId
        { model with creating = false }, Navigation.newUrl datasetUrl
    | CreateResponse (Error err) ->
        { model with creating = false}, Cmd.none
    | _ ->
        model, Cmd.none

let view model dispatch =
    let pageHeader =
        header [ classList [("flex__box", true)] ]
            [ p [ classList [("flex__item", true)] ] [ a [ Href (sprintf "/datasets/%d" model.datasetId) ] [ str "  <  " ]; str "Dataset" ] ] 

    let typeSelect =
        let optionView (taskType: TaskType) =
            let text =
                match taskType.key with
                | TaskTypeKey.Label -> "Label"
            option [ Value (taskType.id.ToString()) ] [ str text ]

        let chooseTaskType (taskTypeId: string) =
            let taskType: TaskType =
                List.find (fun taskType -> taskType.id.ToString() = taskTypeId) model.taskTypes.items
            ChooseTaskType taskType

        let options =
            List.map optionView model.taskTypes.items
        select [ ClassName "form__control"; OnChange (fun evt -> dispatch (chooseTaskType evt.Value)) ] options

    let labelsSelect =
        let isSelected (label: Label) =
            List.exists (fun (_label: Label) -> label.id = _label.id) model.choosedLabels

        let labelView (label: Label) =
            div [ ClassName "checkbox__item" ]
              [ input [ Checked (isSelected label); Type "checkbox"; OnChange (fun evt -> dispatch (ToggleTaskLabel label) ) ]
                span [] [ str label.title ] ]

        let labels =
            List.map labelView model.labels.items

        div [] labels

    let slicesSelect =
        let isSelected (slice: DatasetSlice) =
            List.exists (fun (_slice: DatasetSlice) -> slice.id = _slice.id) model.choosedSlices

        let sliceView (slice: DatasetSlice) =
            div [ClassName "checkbox__item"]
              [ input [ Type "checkbox"; Checked (isSelected slice); OnChange (fun evt -> dispatch (ToggleDatasetSlice slice) )]
                span [] [ str slice.title ] ]

        let slices =
            List.map sliceView model.slices.items

        div [] slices

    let submit (evt: FormEvent) =
        evt.preventDefault()
        dispatch Submit

    let taskCreateForm =
        form [ OnSubmit submit; ClassName "form form--vertical" ]
            [ div [ ClassName "form__field" ]
                  [ h4 [] [ str "Task Type" ]; typeSelect ]
              div []
                  [ h4 [] [ str "Labels" ]; labelsSelect ]
              div []
                  [ h4 [] [ str "Dataset Slices" ]; slicesSelect ]
              div [ ClassName "form__field" ] [ button [ Disabled model.creating; ClassName "button button--fullwidth button--solid button--primary" ] [ str "Create Task" ] ] ]

    div [] [ pageHeader; taskCreateForm ]
