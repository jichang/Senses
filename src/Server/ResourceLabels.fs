module ResourceLabels

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net


module Model =
    let resourceLabelsTable =
        Map.ofList
            [ "id", SqlType.Integer
              "user_id", SqlType.Bigint
              "resource_id", SqlType.Bigint
              "dataset_id", SqlType.Bigint
              "task_id", SqlType.Bigint
              "label_id", SqlType.Integer
              "label_title", SqlType.CharacterVaring
              "label_color", SqlType.CharacterVaring
              "label_status", SqlType.Integer
              "shape", SqlType.Json
              "status", SqlType.Integer ]

    let select (user: User) (datasetId: int64) (taskId: int64) (resourceId: int64): ModelCollection<ResourceLabel> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint resourceLabelsTable
        let statement = """
        SELECT
          labels.id as label_id,
          labels.title as label_title,
          labels.color as label_color,
          labels.status as label_status,
          resource_labels.shape as shape,
          count(*) OVER() AS total_count
        FROM senses.resource_labels as resource_labels
        LEFT JOIN senses.labels as labels ON resource_labels.label_id = labels.id
        WHERE resource_labels.user_id=@user_id AND resource_labels.resource_id=@resource_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id); ("resource_id", Bigint resourceId)]
              columnTypes = columnTypes }
        let rows = Database.execute Database.defaultConnection sql

        match List.isEmpty rows with
        | true ->
            { totalCount = 0L; items = [] }
        | false ->
            let row = List.head rows
            match row.Item "total_count" with
            | Bigint totalCount ->
                let labels =
                    [ for row in rows do
                        let res =
                            let labelIdColumn = row.Item "label_id"
                            let labelColorColumn = row.Item "label_color"
                            let labelTitleColumn = row.Item "label_title"
                            let labelStatusColumn = row.Item "label_status"
                            let shapeColumn = row.Item "shape"
                            match labelIdColumn, labelColorColumn, labelTitleColumn, labelStatusColumn, shapeColumn with
                            | Integer id, CharacterVaring color, CharacterVaring title, Integer status, Json shapeJson ->
                                match  Decode.Auto.fromString<Shape> shapeJson with
                                | Ok shape ->
                                    let label = { id = id; color = color; title = title; status = status }
                                    Ok { label = label; shape = shape }
                                | Error reason ->
                                    Error (Exception reason)
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok label -> yield label
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = labels }
            | _ ->
                { totalCount = 0L; items = [] }

    let create (user: User) (resourceId: int64) (createParams: ResourceLabelsCreateParams) : ResourceLabel list =
        let statement = """
            INSERT INTO senses.resource_labels(user_id, resource_id, label_id, shape)
            VALUES (@user_id, @resource_id, @label_id, @shape)
        """

        let resourceLabels =
            [ for item in createParams.labels do
                let jsonShape = Encode.Auto.toString (0, item.shape)
                let sql =
                    { statement = statement
                      parameters = [("user_id", Bigint user.id); ("resource_id", Bigint resourceId); ("label_id", Integer item.label.id); ("shape", Json jsonShape)]
                      columnTypes = resourceLabelsTable }
                let rows = Database.execute Database.defaultConnection sql
                match List.length rows with
                | 1 ->
                    yield item
                | _ ->
                    printfn ""
                ]

        resourceLabels

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (datasetId: int64) (taskId: int64) (resourceId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let labels: ModelCollection<ResourceLabel> = Model.select user datasetId taskId resourceId
            return! Controller.json ctx labels
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let createAction (datasetId: int64) (taskId: int64) (resourceId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let! body = Controller.getJson<ResourceLabelsCreateParams> ctx
            let resourceLabels: ResourceLabel list = Model.create user resourceId body
            return! Controller.json ctx resourceLabels
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }


let controller (datasetId: int64) (taskId: int64) (resourceId: int64) = controller {
    index (Controller.indexAction datasetId taskId resourceId)
    create (Controller.createAction datasetId taskId resourceId)
}
