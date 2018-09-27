module Tasks

open Elmish
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Shared.Model
open Elmish.Browser.Navigation

type Model =
    { loading: bool
      totalCount: int64
      tasks: Task list }

type Msg =
    | Init of Result<ModelCollection<Task>, exn>
    | Choose of Task
    | Create

let init () =
    let model =
        { loading = true
          totalCount = 0L
          tasks = List.empty }

    let session: Result<Session, string> = Token.load ()
    match session with
    | Ok session ->
        let authorization = sprintf "Bearer %s" session.token

        let defaultProps =
            [ RequestProperties.Method HttpMethod.GET
              requestHeaders
                  [ ContentType "application/json" 
                    Authorization authorization ]]

        let cmd =
            let decoder =(ModelCollection<Task>.Decoder Task.Decoder)
            Cmd.ofPromise
                (fun _ -> fetchAs "/api/tasks" decoder defaultProps)
                ()                
                (Ok >> Init)
                (Error >> Init)
        model, cmd
    | Error _ ->
        model, Cmd.none

let update (msg: Msg) (model: Model) =
    match msg with
    | Init (Ok response) ->
        { model with totalCount = response.totalCount; tasks = response.items; loading = false }, Cmd.none
    | Init (Error exn) ->
        { model with loading = false }, Navigation.newUrl "/sign"
    | Choose task ->
        let taskUrl = sprintf "/tasks/%i" task.id
        model, Navigation.newUrl taskUrl
    | Create ->
        model, Navigation.newUrl "/tasks/create"


let view (model: Model) (dispatch: Msg -> unit) =
    if model.loading then
        div [] [ str "loading" ]
    else
        if Seq.isEmpty model.tasks then
            div []
                [ div []
                    [ p [] [ str "No tasks yet" ]
                      a [ (OnClick (fun evt -> dispatch Create))] [ str "Create Task" ]] ]
        else
            let taskRow (task: Task) =
                let taskId = task.id.ToString()
                tr []
                    [ td [] [ str taskId ]
                      td [] [ button [ OnClick (fun _ -> dispatch (Choose task)) ] [ str "Details" ] ] ]

            let tasksRow =
                Seq.map taskRow model.tasks
                |> Seq.toList

            let tasksTable =
                table []
                    [ tbody [] tasksRow ]

            let tableHeader =
                header []
                    [ p [] [ str "Tasks" ]
                      button [ OnClick (fun _ -> dispatch Create )] [ str "Create" ] ] 

            div []
                [ div [] [ tableHeader; tasksTable] ]
