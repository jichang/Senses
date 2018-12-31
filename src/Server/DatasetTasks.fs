module DatasetTasks

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net


module Model =
    let tasksTable =
        Map.ofList
            [ "id", SqlType.Bigint
              "user_id", SqlType.Bigint
              "dataset_id", SqlType.Bigint
              "dataset_slice_id", SqlType.Bigint
              "type_id", SqlType.Integer
              "type_key", SqlType.CharacterVaring
              "type_status", SqlType.Integer
              "status", SqlType.Integer ]

    let taskLabelsTable = 
        Map.ofList
            [ "dataset_task_id", SqlType.Bigint
              "id", SqlType.Integer
              "label_id", SqlType.Integer
              "label_title", SqlType.CharacterVaring
              "label_color", SqlType.CharacterVaring
              "label_status", SqlType.Integer ]

    let taskDatasetSlicesTable = 
        Map.ofList
            [ "dataset_task_id", SqlType.Bigint
              "id", SqlType.Bigint
              "dataset_slice_id", SqlType.Bigint
              "dataset_slice_title", SqlType.CharacterVaring
              "dataset_slice_status", SqlType.Integer ]

    let selectAll (user: User) (datasetId: int64): ModelCollection<Task> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint tasksTable

        let statement = """
        SELECT tasks.id as id, tasks.status as status, types.id as type_id, types.key as type_key, types.status as type_status, count(*) OVER() AS total_count
        FROM senses.dataset_tasks as tasks
        LEFT JOIN senses.task_types as types ON tasks.task_type_id = types.id
        LEFT JOIN senses.datasets as datasets ON tasks.dataset_id = datasets.id
        WHERE datasets.user_id=@user_id AND datasets.id=@dataset_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id);("dataset_id", Bigint datasetId)]
              columnTypes = columnTypes }
        let rows = Database.execute Database.defaultConnection sql

        match List.isEmpty rows with
        | true ->
            { totalCount = 0L; items = [] }
        | false ->
            let row = List.head rows
            match row.Item "total_count" with
            | Bigint totalCount ->
                let tasks =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let typeIdColumn = row.Item "type_id"
                            let typeKeyColumn = row.Item "type_key"
                            let typeStatusColumn = row.Item "type_status"
                            let statusColumn = row.Item "status"
                            match idColumn, typeIdColumn, typeKeyColumn, typeStatusColumn, statusColumn with
                            | Bigint id, Integer typeId, CharacterVaring "label", Integer typeStatus, Integer status ->
                                let taskLabelsStatement = """
                                SELECT labels.id as label_id, labels.title as label_title, labels.color as label_color, labels.status as label_status
                                FROM senses.labels as labels
                                LEFT JOIN senses.task_labels as task_labels ON labels.id = task_labels.label_id
                                WHERE task_labels.dataset_task_id=@dataset_task_id
                                """

                                let taskLabelsSql =
                                    { statement = taskLabelsStatement
                                      parameters = [("dataset_task_id", Bigint id)]
                                      columnTypes = taskLabelsTable }
                                let taskLabelsRows = Database.execute Database.defaultConnection taskLabelsSql
                                let taskLabels =
                                    [ for row in taskLabelsRows do
                                        let idColumn = row.Item "label_id"
                                        let titleColumn = row.Item "label_title"
                                        let colorColumn = row.Item "label_color"
                                        let statusColumn = row.Item "label_status"
                                        match idColumn, titleColumn, colorColumn, statusColumn with
                                        | Integer id, CharacterVaring title, CharacterVaring color, Integer status ->
                                            yield { id = id; title = title; color = color; status = status}
                                        | _ ->
                                            printfn "unsuppored column type" ]

                                let taskDatasetSlicesStatement = """
                                SELECT dataset_slices.id as dataset_slice_id, dataset_slices.title as dataset_slice_title, dataset_slices.status as dataset_slice_status
                                FROM senses.dataset_slices as dataset_slices
                                LEFT JOIN senses.task_dataset_slices as task_dataset_slices ON dataset_slices.id = task_dataset_slices.dataset_slice_id
                                WHERE task_dataset_slices.dataset_task_id=@dataset_task_id
                                """

                                let taskDatasetSlicesSql =
                                    { statement = taskDatasetSlicesStatement
                                      parameters = [("dataset_task_id", Bigint id)]
                                      columnTypes = taskDatasetSlicesTable }
                                let taskDatasetSlicesRows = Database.execute Database.defaultConnection taskDatasetSlicesSql
                                let taskDatasetSlices =
                                    [ for row in taskDatasetSlicesRows do
                                        let idColumn = row.Item "dataset_slice_id"
                                        let titleColumn = row.Item "dataset_slice_title"
                                        let statusColumn = row.Item "dataset_slice_status"
                                        match idColumn, titleColumn,statusColumn with
                                        | Bigint id, CharacterVaring title, Integer status ->
                                            yield { id = id; title = title; status = status}
                                        | _ ->
                                            printfn "unsuppored column type" ]

                                let task =
                                    { id = id
                                      ``type`` = 
                                          { id = typeId
                                            key = TaskTypeKey.Label
                                            status = typeStatus}
                                      labels = { totalCount = int64(List.length taskLabels); items = taskLabels }
                                      datasetSlices = { totalCount = int64(List.length taskDatasetSlices); items = taskDatasetSlices }
                                      status = status}
                                Ok task
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok task -> yield task
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = tasks }
            | _ ->
                { totalCount = 0L; items = [] }

    // this better runs in the same transaction
    let create (user: User) (datasetId: int64) (createParams: TaskCreateParams) =
        let statement = """
        INSERT INTO senses.dataset_tasks(task_type_id, dataset_id)
        VALUES (
            @task_type_id,
            (SELECT id from senses.datasets WHERE id=@dataset_id AND user_id=@user_id)
        )
        RETURNING id
        """
        let sql =
            { statement = statement
              parameters =
                  [ ("dataset_id", Bigint datasetId)
                    ("task_type_id", Integer createParams.taskType.id)
                    ("user_id", Bigint user.id) ]
              columnTypes = tasksTable }
        let rows = Database.execute Database.defaultConnection sql

        if rows.Length = 1 then
            let row = rows.[0]
            let idColumn = row.Item "id"
            match idColumn with
            | Bigint id ->
                let taskLabels =
                    [ for label in createParams.labels do
                        let statement = """
                        INSERT INTO senses.task_labels(dataset_task_id, label_id)
                        VALUES (@dataset_task_id, @label_id)
                        RETURNING id
                        """
                        let sql =
                            { statement = statement 
                              parameters = [("dataset_task_id", Bigint id); ("label_id", Integer label.id)]
                              columnTypes = taskLabelsTable }
                        let rows = Database.execute Database.defaultConnection sql
                        if rows.Length = 1 then
                            let row = rows.[0]
                            let idColumn = row.Item "id"
                            match idColumn with
                            | Bigint id -> yield label
                            | _ -> printfn "unexpetec error"
                        else
                            printfn "unexpected error" ]

                let taskSlices =
                    [ for slice in createParams.datasetSlices do
                        let statement = """
                        INSERT INTO senses.task_dataset_slices(dataset_task_id, dataset_slice_id)
                        VALUES (@dataset_task_id, @dataset_slice_id)
                        RETURNING id
                        """
                        let sql =
                            { statement = statement 
                              parameters = [("dataset_task_id", Bigint id); ("dataset_slice_id", Bigint slice.id)]
                              columnTypes = taskDatasetSlicesTable }
                        let rows = Database.execute Database.defaultConnection sql
                        if rows.Length = 1 then
                            let row = rows.[0]
                            let idColumn = row.Item "id"
                            match idColumn with
                            | Bigint id -> yield slice
                            | _ -> printfn "unexpetec error"
                        else
                            printfn "unexpected error" ]

                let task: Task = {
                    id = id
                    ``type`` = createParams.taskType
                    labels = { totalCount = int64(taskLabels.Length); items = taskLabels}
                    datasetSlices = { totalCount = int64(taskSlices.Length); items = taskSlices}
                    status = 0 }

                Ok task
            | _ ->
                Error (Exception "unmatch column type")
        else
            Error (Exception "unexpected error")

    let selectOne (user: User) (datasetId: int64) (taskId: int64) : Result<Task, exn> =
        let statement = """
        SELECT tasks.id as id, tasks.status as status, types.id as type_id, types.key as type_key, types.status as type_status
        FROM senses.dataset_tasks as tasks
        LEFT JOIN senses.datasets as datasets ON datasets.id = tasks.dataset_id
        LEFT JOIN senses.task_types as types ON types.id = tasks.task_type_id
        WHERE datasets.user_id=@user_id AND datasets.id=@dataset_id AND tasks.id=@task_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id); ("dataset_id", Bigint datasetId); ("task_id", Bigint taskId)]
              columnTypes = tasksTable }
        let rows = Database.execute Database.defaultConnection sql

        match rows.Length with
        | 1 ->
            let row = List.head rows
            let idColumn = row.Item "id"
            let statusColumn = row.Item "status"
            let typeIdColumn = row.Item "type_id"
            let typeKeyColumn = row.Item "type_key"
            let typeStatusColumn = row.Item "type_status"
            match idColumn, statusColumn, typeIdColumn, typeKeyColumn, typeStatusColumn with
            | Bigint id, Integer status, Integer typeId, CharacterVaring "label", Integer typeStatus ->
                let taskLabelsStatement = """
                SELECT labels.id as label_id, labels.title as label_title, labels.color as label_color, labels.status as label_status
                FROM senses.task_labels as task_labels
                LEFT JOIN senses.labels as labels ON labels.id = task_labels.label_id
                WHERE task_labels.dataset_task_id=@dataset_task_id
                """

                let taskLabelsSql =
                    { statement = taskLabelsStatement
                      parameters = [("dataset_task_id", Bigint taskId)]
                      columnTypes = taskLabelsTable }
                let taskLabelsRows = Database.execute Database.defaultConnection taskLabelsSql
                let taskLabels =
                    [ for row in taskLabelsRows do
                        let idColumn = row.Item "label_id"
                        let titleColumn = row.Item "label_title"
                        let colorColumn = row.Item "label_color"
                        let statusColumn = row.Item "label_status"
                        match idColumn, titleColumn, colorColumn, statusColumn with
                        | Integer id, CharacterVaring title, CharacterVaring color, Integer status ->
                            yield { id = id; title = title; color = color; status = status}
                        | _ ->
                            printfn "unsuppored column type" ]

                let taskDatasetSlicesStatement = """
                SELECT dataset_slices.id as dataset_slice_id, dataset_slices.title as dataset_slice_title, dataset_slices.status as dataset_slice_status
                FROM senses.dataset_slices as dataset_slices
                LEFT JOIN senses.task_dataset_slices as task_dataset_slices ON dataset_slices.id = task_dataset_slices.dataset_slice_id
                WHERE task_dataset_slices.dataset_task_id=@dataset_task_id
                """

                let taskDatasetSlicesSql =
                    { statement = taskDatasetSlicesStatement
                      parameters = [("dataset_task_id", Bigint taskId)]
                      columnTypes = taskDatasetSlicesTable }
                let taskDatasetSlicesRows = Database.execute Database.defaultConnection taskDatasetSlicesSql
                let taskDatasetSlices =
                    [ for row in taskDatasetSlicesRows do
                        let idColumn = row.Item "dataset_slice_id"
                        let titleColumn = row.Item "dataset_slice_title"
                        let statusColumn = row.Item "dataset_slice_status"
                        match idColumn, titleColumn,statusColumn with
                        | Bigint id, CharacterVaring title, Integer status ->
                            yield { id = id; title = title; status = status}
                        | _ ->
                            printfn "unsuppored column type" ]
                let task =
                    { id = id
                      ``type`` = { id = typeId; key = TaskTypeKey.Label; status = typeStatus }
                      labels = { totalCount = int64(List.length taskLabels); items = taskLabels }
                      datasetSlices = { totalCount = int64(List.length taskDatasetSlices); items = taskDatasetSlices }
                      status = status }
                Ok task
            | _ ->
                Error (Exception ("unmatch column value"))
        | _ ->
            Error (Exception "No database found")

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (datasetId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let tasks: ModelCollection<Task> = Model.selectAll user datasetId
            return! Controller.json ctx tasks
        | Error err ->
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let createAction (datasetId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let! createParams = Controller.getJson<TaskCreateParams> ctx
            match Model.create user datasetId createParams with
            | Ok task ->
                return! Controller.json ctx task
            | Error _ ->
                return! Controller.json ctx ()
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let showAction (datasetId: int64) (ctx: HttpContext) (id: int64) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            match Model.selectOne user datasetId id with
            | Ok task ->
                return! Controller.json ctx task
            | Error _ ->
                return! Controller.text ctx ""
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    module Resources =
        module Model =
            let resourcesTable =
                Map.ofList
                    [ "id", SqlType.Bigint
                      "user_id", SqlType.Bigint
                      "dataset_id", SqlType.Bigint
                      "dataset_slice_id", SqlType.Bigint
                      "dataset_task_id", SqlType.Bigint
                      "type_id", SqlType.Integer
                      "type_key", SqlType.CharacterVaring
                      "type_status", SqlType.Integer
                      "uri", SqlType.CharacterVaring
                      "content", SqlType.Text
                      "status", SqlType.Integer ]

            let selectAll (user: User) (datasetId: int64) (taskId: int64): ModelCollection<Resource> =
                let columnTypes =
                    Map.add "total_count" SqlType.Bigint resourcesTable

                let statement = """
                SELECT
                  resources.id as id,
                  resources.uri as uri,
                  resources.content as content,
                  resources.status as status,
                  types.id as type_id,
                  types.key as type_key,
                  types.status as type_status,
                  count(*) OVER() AS total_count
                FROM senses.dataset_tasks as tasks
                LEFT JOIN senses.task_dataset_slices as task_slices ON tasks.id = task_slices.dataset_task_id
                LEFT JOIN senses.dataset_slices as slices ON slices.id = task_slices.dataset_slice_id
                LEFT JOIN senses.datasets as datasets ON slices.dataset_id = datasets.id
                LEFT JOIN senses.resources as resources ON resources.dataset_slice_id = slices.id
                LEFT JOIN senses.resource_types as types ON resources.resource_type_id = types.id
                WHERE datasets.user_id=@user_id AND tasks.dataset_id=@dataset_id AND tasks.id=@task_id
                """
                let sql =
                    { statement = statement
                      parameters = [("user_id", Bigint user.id); ("task_id", Bigint taskId); ("dataset_id", Bigint datasetId)]
                      columnTypes = columnTypes }
                let rows = Database.execute Database.defaultConnection sql

                match List.isEmpty rows with
                | true ->
                    { totalCount = 0L; items = [] }
                | false ->
                    let row = List.head rows
                    match row.Item "total_count" with
                    | Bigint totalCount ->
                        let resources =
                            [ for row in rows do
                                let typeIdColumn = row.Item "type_id"
                                let typeKeyColumn = row.Item "type_key"
                                let typeStatusColumn = row.Item "type_status"
                                let idColumn = row.Item "id"
                                let uriColumn = row.Item "uri"
                                let contentColumn = row.Item "content"
                                let statusColumn = row.Item "status"
                                let res =
                                    match idColumn, uriColumn, contentColumn, statusColumn, typeIdColumn, typeKeyColumn, typeStatusColumn with
                                    | Bigint id, CharacterVaring uri, Text content, Integer status, Integer typeId, CharacterVaring "video", Integer typeStatus ->
                                        Ok { id = id; uri = uri; content = content; status = status; ``type`` = { id = typeId; key = ResourceTypeKey.Video; status = typeStatus } }
                                    | Bigint id, CharacterVaring uri, Text content, Integer status, Integer typeId, CharacterVaring "image", Integer typeStatus ->
                                        Ok { id = id; uri = uri; content = content; status = status; ``type`` = { id = typeId; key = ResourceTypeKey.Image; status = typeStatus } }
                                    | Bigint id, CharacterVaring uri, Text content, Integer status, Integer typeId, CharacterVaring "text", Integer typeStatus ->
                                        Ok { id = id; uri = uri; content = content; status = status; ``type`` = { id = typeId; key = ResourceTypeKey.Text; status = typeStatus } }
                                    | res ->
                                        let msg = sprintf "unmatch column type: %A" res
                                        Error msg

                                match res with
                                | Ok resource -> yield resource
                                | Error err -> printfn "%A" err ]

                        { totalCount = totalCount; items = resources }
                    | _ ->
                        { totalCount = 0L; items = [] }

        module Controller =
            let indexAction (datasetId: int64) (taskId: int64) (ctx: HttpContext) = task {
                let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
                match Decode.fromString User.Decoder sub.Value with
                | Ok user ->
                    let users: ModelCollection<Resource> = Model.selectAll user datasetId taskId
                    return! Controller.json ctx users
                | Error err ->
                    let response =
                        { code = "invalid user code"}
                    return! Controller.json ctx response
            }

        let controller (datasetId: int64) (taskId: int64) = controller {
            index (Controller.indexAction datasetId taskId)
        }

let controller (datasetId: int64) = controller {
    subController "/resources" (Controller.Resources.controller datasetId)

    index (Controller.indexAction datasetId)
    create (Controller.createAction datasetId)
    show (Controller.showAction datasetId)
}
