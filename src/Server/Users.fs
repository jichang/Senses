module Users

open System
open Shared.Model
open Saturn
open Database


module Model =
    let usersTable =
        Map.ofList
            [ "id", SqlType.Bigint
              "uuid", SqlType.Uuid
              "status", SqlType.Integer ]

    let select (): ModelCollection<User> =
        let columnTypes = Map.add "total_count" SqlType.Bigint usersTable
        let sql =
            { statement = "SELECT id, uuid, status, count(*) OVER() AS total_count from senses.users"
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
                let users =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let uuidColumn = row.Item "uuid"
                            let statusColumn = row.Item "status"
                            match idColumn, uuidColumn, statusColumn with
                            | Bigint id, Uuid uuid, Integer status ->
                                Ok { id = id; uuid = uuid; status = status }
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok user -> yield user
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = users }
            | _ ->
                { totalCount = 0L; items = [] }

    let create () =
        let uuid = Guid.NewGuid()
        let sql =
            { statement = "INSERT INTO senses.users(uuid) VALUES (@uuid) RETURNING id"
              parameters = [("uuid", Uuid uuid)]
              columnTypes = usersTable }
        let rows = Database.execute Database.defaultConnection sql

        [ for row in rows do
            let res =
                let idColumn = row.Item "id"
                match idColumn with
                | Bigint id ->
                    Ok { id = id; uuid = uuid; status = 0 }
                | _ ->
                    Error (Exception ("unmatch column type"))
            match res with
            | Ok user -> yield user
            | Error err -> printfn "%A" err ]

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction ctx = task {
        let users: ModelCollection<User> = Model.select ()
        return! Controller.json ctx users
    }

    let createAction ctx = task {
        let user: User = List.head (Model.create ())
        let token = Token.generate user
        return! Controller.json ctx { token = token }
    }

let controller = controller {
    index Controller.indexAction
    create Controller.createAction
}
