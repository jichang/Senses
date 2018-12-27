module TaskTypes

open System
open Shared.Model
open Saturn
open Database


module Model =
    let taskTypesTable =
        Map.ofList
            [ "id", SqlType.Integer
              "key", SqlType.CharacterVaring
              "status", SqlType.Integer ]

    let selectAll (): ModelCollection<TaskType> =
        let columnTypes = Map.add "total_count" SqlType.Bigint taskTypesTable
        let sql =
            { statement = "SELECT id, key, status, count(*) OVER() AS total_count from senses.task_types"
              parameters = []
              columnTypes = columnTypes }
        let rows = Database.execute Database.defaultConnection sql

        match List.isEmpty rows with
        | true ->
            { totalCount = 0L; items = [] }
        | false ->
            let row = List.head rows
            match row.Item "total_count" with
            | Bigint totalCount ->
                let taskTypes =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let keyColumn = row.Item "key"
                            let statusColumn = row.Item "status"
                            match idColumn, keyColumn, statusColumn with
                            | Integer id, CharacterVaring "label", Integer status ->
                                Ok { id = id; key = TaskTypeKey.Label; status = status }
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok taskType -> yield taskType
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = taskTypes }
            | _ ->
                { totalCount = 0L; items = [] }

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction ctx = task {
        let taskTypes: ModelCollection<TaskType> = Model.selectAll ()
        return! Controller.json ctx taskTypes
    }


let controller = controller {
    index Controller.indexAction
}
