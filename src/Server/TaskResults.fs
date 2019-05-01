module TaskResults

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net

module Model =
    let taskResultsTable =
        Map.ofList
            [ "id", SqlType.Bigint
              "user_id", SqlType.Bigint
              "dataset_id", SqlType.Bigint
              "dataset_slice_id", SqlType.Bigint
              "dataset_task_id", SqlType.Bigint
              "label_title", SqlType.Option SqlType.CharacterVaring
              "uri", SqlType.CharacterVaring
              "content", SqlType.Text
              "shape", SqlType.Option SqlType.Json ]

    let selectAll (user: User) (datasetId: int64) (taskId: int64): ResourceLabels list =
        let statement = """
            SELECT
                resources.id as id,
                resources.uri as uri,
                resources.content as content,
                resource_labels.shape as shape,
                labels.title as label_title
            FROM senses.datasets as datasets
            LEFT JOIN senses.dataset_tasks as tasks ON tasks.dataset_id = datasets.id
            LEFT JOIN senses.task_dataset_slices as task_slices ON tasks.id = task_slices.dataset_task_id
            LEFT JOIN senses.dataset_slices as slices ON slices.id = task_slices.dataset_slice_id
            LEFT JOIN senses.resources as resources ON resources.dataset_slice_id = slices.id
            LEFT JOIN senses.resource_labels as resource_labels ON resource_labels.resource_id = resources.id
            LEFT JOIN senses.labels as labels ON resource_labels.label_id = labels.id
            WHERE datasets.user_id=@user_id AND tasks.dataset_id=@dataset_id AND tasks.id=@task_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id); ("task_id", Bigint taskId); ("dataset_id", Bigint datasetId)]
              columnTypes = taskResultsTable }
        let rows = Database.execute Database.defaultConnection sql
        let resourceRows = Seq.groupBy (fun (row: Row)  -> row.Item "id") rows
        [ for (resourceId, rows) in resourceRows do
            let resourceLabels =
                [ for row in rows do
                    let uriColumn = row.Item "uri"
                    let contentColumn = row.Item "content"
                    let shapeColumn = row.Item "shape"
                    let labelTitleColumn = row.Item "label_title"
                    match uriColumn, contentColumn, shapeColumn, labelTitleColumn with
                    | CharacterVaring uri, Text content, Json shapeJson, CharacterVaring labelTitle ->
                        match  Decode.fromString Shape.Decoder shapeJson with
                        | Ok shape ->
                            let result = {
                                uri = uri
                                content = content
                                results = [{ title = labelTitle; shape = shape }]
                            }
                            yield result
                        | _ ->
                            printfn "incorrect shape"
                    | _ ->
                        printfn "unmatch column type"
                ]
            let results =
                Seq.map (fun resourceLabel -> resourceLabel.results) resourceLabels
                |> Seq.concat
                |> Seq.toList

            match Seq.tryHead resourceLabels with
            | Some resourceLabel ->
                yield { resourceLabel with results = results }
            | _ ->
                printfn "no labels"
        ]

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (datasetId: int64) (taskId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let users: ResourceLabels list = Model.selectAll user datasetId taskId
            return! Controller.json ctx users
        | Error err ->
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

let controller (datasetId: int64) (taskId: int64) = controller {
    index (Controller.indexAction datasetId taskId)
}
