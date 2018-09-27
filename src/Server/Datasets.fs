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

    let select (user: User): ModelCollection<Dataset> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint datasetsTable
        let sql =
            { statement = "SELECT id, title, status, count(*) OVER() AS total_count FROM senses.datasets WHERE user_id=@user_id"
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
                                Ok { id = id; title = title; user = user; tasks = { totalCount = 0L; items = [] }; resources = { totalCount = 0L; items = [] }; status = status}
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
                    Ok { id = id; title = title; user = user; tasks = { totalCount = 0L; items = [] }; resources = { totalCount = 0L; items = [] }; status = 0}
                | _ ->
                    Error (Exception ("unmatch column type"))
            match res with
            | Ok dataset -> yield dataset
            | Error err -> printfn "%A" err ]

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let users: ModelCollection<Dataset> = Model.select user
            return! Controller.json ctx users
        | Error err ->
            printfn "error parsing claim: %A" err
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


let controller = controller {
    index Controller.indexAction
    create Controller.createAction
}
