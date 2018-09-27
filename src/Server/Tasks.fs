module Tasks

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
              "status", SqlType.Integer 
              "task_type_id", SqlType.Integer
              "task_type_key", SqlType.CharacterVaring
              "task_type_status", SqlType.Integer ]

    let select (user: User): ModelCollection<Task> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint tasksTable
        let sql =
            { statement =
                """
                SELECT
                    id, status, count(*) OVER() AS total_count,
                    task_type_id, task_type_key, task_type_status
                FROM senses.tasks
                LEFT JOIN senses.task_types ON senses.tasks.task_type_id= senses.task_types.id
                WHERE user_id=@user_id
                """
              parameters = [("user_id", Bigint user.id)]
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
                            let statusColumn = row.Item "status"
                            let taskTypeIdColumn = row.Item "task_type_id"
                            let taskTypeKeyColumn = row.Item "task_type_key"
                            let taskTypeStatusColumn = row.Item "task_type_status"
                            match idColumn, statusColumn, taskTypeIdColumn, taskTypeKeyColumn, taskTypeStatusColumn with
                            | Bigint id, Integer status, Integer taskTypeId, CharacterVaring "label", Integer taskTypeStatus ->
                                let taskType =
                                    { id = taskTypeId
                                      key = TaskTypeKey.Label
                                      status = taskTypeStatus }
                                Ok { id = id; ``type`` = taskType; status = status }
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok task -> yield task
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = tasks }
            | _ ->
                { totalCount = 0L; items = [] }

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let users: ModelCollection<Task> = Model.select user
            return! Controller.json ctx users
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }


let controller = controller {
    index Controller.indexAction
}
