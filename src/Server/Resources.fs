module Resources

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
              "type_id", SqlType.Integer
              "type_key", SqlType.CharacterVaring
              "type_status", SqlType.Integer
              "uri", SqlType.CharacterVaring
              "content", SqlType.Text
              "status", SqlType.Integer ]

    let create (user: User) (datasetId: int64) (datasetSliceId: int64) (createParams: ResourceFileLine) =
        let typeKey =
            match createParams.``type`` with
            | ResourceTypeKey.Text -> "text"
            | ResourceTypeKey.Image -> "image"
            | ResourceTypeKey.Video -> "video"

        let statement = """
        INSERT INTO senses.resources(dataset_slice_id, resource_type_id, uri, content)
        VALUES (
            (SELECT id FROM senses.dataset_slices as slices WHERE slices.id=@dataset_slice_id AND slices.dataset_id=@dataset_id AND slices.user_id=@user_id),
            (SELECT id from senses.resource_types where key = @type_key),
            @uri,
            @content
        )
        RETURNING id
        """
        let sql =
            { statement = statement
              parameters =
                  [ ("dataset_id", Bigint datasetId)
                    ("dataset_slice_id", Bigint datasetSliceId)
                    ("user_id", Bigint user.id)
                    ("type_key", CharacterVaring typeKey)
                    ("uri", CharacterVaring createParams.source.uri)
                    ("content", CharacterVaring createParams.source.content) ]
              columnTypes = resourcesTable }
        let rows = Database.execute Database.defaultConnection sql
        if rows.Length = 1 then
            let row = rows.[0]
            let idColumn = row.Item "id"
            match idColumn with
            | Bigint id ->
                Ok id
            | _ ->
                Error (Exception "unsupported column type")
        else
            Error (Exception "unexpected error")

    let selectAll (user: User) (datasetId: int64) (datasetSliceId: int64): ModelCollection<Resource> =
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
        FROM senses.resources as resources
        LEFT JOIN senses.resource_types as types ON resources.resource_type_id = types.id
        LEFT JOIN senses.dataset_slices as slices ON slices.id = resources.dataset_slice_id
        LEFT JOIN senses.datasets as datasets ON slices.dataset_id = datasets.id
        WHERE datasets.user_id=@user_id AND resources.dataset_slice_id=@dataset_slice_id AND datasets.id=@dataset_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id); ("dataset_slice_id", Bigint datasetSliceId); ("dataset_id", Bigint datasetId)]
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

    let indexAction (datasetId: int64) (datasetSliceId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let users: ModelCollection<Resource> = Model.selectAll user datasetId datasetSliceId
            return! Controller.json ctx users
        | Error err ->
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

let controller (datasetId: int64) (datasetSliceId: int64) = controller {
    index (Controller.indexAction datasetId datasetSliceId)
}
