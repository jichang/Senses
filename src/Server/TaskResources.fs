module TaskResources

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net

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
    open FSharp.Control.Tasks.ContextInsensitive

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
    subController "/labels" (ResourceLabels.controller datasetId taskId)

    index (Controller.indexAction datasetId taskId)
}
