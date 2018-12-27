module Datasets

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net


module Model =
    let datasetsTable =
        Map.ofList
            [ "id", SqlType.Bigint
              "title", SqlType.CharacterVaring
              "user_id", SqlType.Bigint
              "status", SqlType.Integer ]

    let selectAll (user: User): ModelCollection<Dataset> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint datasetsTable
        let statement = """
        SELECT id, title, status, count(*) OVER() AS total_count FROM senses.datasets WHERE user_id=@user_id
        """
        let sql =
            { statement = statement
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
                let datasets =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let titleColumn = row.Item "title"
                            let statusColumn = row.Item "status"
                            match idColumn, titleColumn, statusColumn with
                            | Bigint id, CharacterVaring title, Integer status ->
                                let slices = DatasetSlices.Model.selectAll user id
                                Ok { id = id; title = title; user = user; tasks = { totalCount = 0L; items = [] }; slices = slices; status = status}
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok dataset -> yield dataset
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = datasets }
            | _ ->
                { totalCount = 0L; items = [] }

    let create (title: string) (user: User) : Dataset list =
        let sql =
            { statement = "INSERT INTO senses.datasets(user_id, title) VALUES (@user_id, @title) RETURNING id"
              parameters = [("title", CharacterVaring title); ("user_id", Bigint user.id)]
              columnTypes = datasetsTable }
        let rows = Database.execute Database.defaultConnection sql

        [ for row in rows do
            let res =
                let idColumn = row.Item "id"
                match idColumn with
                | Bigint id ->
                    Ok { id = id; title = title; user = user; tasks = { totalCount = 0L; items = [] }; slices = { totalCount = 0L; items = [] }; status = 0}
                | _ ->
                    Error (Exception ("unmatch column type"))
            match res with
            | Ok dataset -> yield dataset
            | Error err -> printfn "%A" err ]

    let selectOne (user: User) (datasetId: int64): Result<Dataset, exn> =
        let sql =
            { statement = "SELECT id, title, status FROM senses.datasets WHERE user_id=@user_id AND id=@id"
              parameters = [("user_id", Bigint user.id); ("id", Bigint datasetId)]
              columnTypes = datasetsTable }
        let rows = Database.execute Database.defaultConnection sql

        match rows.Length with
        | 1 ->
            let row = List.head rows
            let idColumn = row.Item "id"
            let titleColumn = row.Item "title"
            let statusColumn = row.Item "status"
            match idColumn, titleColumn, statusColumn with
            | Bigint id, CharacterVaring title, Integer status ->
                let slices = DatasetSlices.Model.selectAll user id
                let tasks = DatasetTasks.Model.selectAll user id
                Ok { id = id; title = title; user = user; tasks = tasks; slices = slices; status = status}
            | _ ->
                Error (Exception ("unmatch column value"))
        | _ ->
            Error (Exception "No database found")

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let users: ModelCollection<Dataset> = Model.selectAll user
            return! Controller.json ctx users
        | Error err ->
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let createAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let! body = Controller.getJson<DatasetCreateParams> ctx
            let dataset: Dataset = List.head (Model.create body.title user)
            return! Controller.json ctx dataset
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let showAction (ctx: HttpContext) (id: int64) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            match Model.selectOne user id with
            | Ok dataset ->
                return! Controller.json ctx dataset
            | Error _ ->
                return! Controller.text ctx ""
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }


let controller = controller {
    subController "/slices" DatasetSlices.controller
    subController "/tasks" DatasetTasks.controller

    index Controller.indexAction
    create Controller.createAction
    show Controller.showAction
}
