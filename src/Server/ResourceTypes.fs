module ResourceTypes

open System
open Shared.Model
open Saturn
open Database


module Model =
    let resourceTypesTable =
        Map.ofList
            [ "id", SqlType.Integer
              "key", SqlType.CharacterVaring
              "status", SqlType.Integer ]

    let select (): ModelCollection<ResourceType> =
        let columnTypes = Map.add "total_count" SqlType.Bigint resourceTypesTable
        let sql =
            { statement = "SELECT id, key, status, count(*) OVER() AS total_count from senses.resource_types"
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
                let resourceTypes =
                    [ for row in rows do
                        let res: Result<ResourceType, exn> =
                            let idColumn = row.Item "id"
                            let keyColumn = row.Item "key"
                            let statusColumn = row.Item "status"
                            match idColumn, keyColumn, statusColumn with
                            | Integer id, CharacterVaring "video", Integer status ->
                                Ok { id = id; key = ResourceTypeKey.Video; status = status }
                            | Integer id, CharacterVaring "audo", Integer status ->
                                Ok { id = id; key = ResourceTypeKey.Image; status = status }
                            | Integer id, CharacterVaring "text", Integer status ->
                                Ok { id = id; key = ResourceTypeKey.Text; status = status }
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok resourceType -> yield resourceType
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = resourceTypes }
            | _ ->
                { totalCount = 0L; items = [] }


module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction ctx = task {
        let resourceTypes: ModelCollection<ResourceType> = Model.select ()
        return! Controller.json ctx resourceTypes
    }


let controller = controller {
    index Controller.indexAction
}
