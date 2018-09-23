module Labels

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net


module Model =
    let labelsTable =
        Map.ofList
            [ "id", SqlType.Integer
              "user_id", SqlType.Bigint
              "color", SqlType.CharacterVaring
              "title", SqlType.CharacterVaring
              "status", SqlType.Integer ]

    let select (user: User): ModelCollection<Label> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint labelsTable
        let sql =
            { statement = "SELECT id, title, color, status, count(*) OVER() AS total_count FROM senses.labels WHERE user_id=@user_id"
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
                let labels =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let colorColumn = row.Item "color"
                            let titleColumn = row.Item "title"
                            let statusColumn = row.Item "status"
                            match idColumn, colorColumn, titleColumn, statusColumn with
                            | Integer id, CharacterVaring color, CharacterVaring title,Integer status ->
                                Ok { id = id; color = color; title = title; status = status }
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok label -> yield label
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = labels }
            | _ ->
                { totalCount = 0L; items = [] }

    let create (color: string) (title: string) (user: User) : Label list =
        let sql =
            { statement = "INSERT INTO senses.labels(user_id, color, title) VALUES (@user_id, @color, @title) RETURNING id"
              parameters = [("color", CharacterVaring color); ("title", CharacterVaring title); ("user_id", Bigint user.id)]
              columnTypes = labelsTable }
        let rows = Database.execute Database.defaultConnection sql

        [ for row in rows do
            let res =
                let idColumn = row.Item "id"
                match idColumn with
                | Integer id ->
                    Ok { id = id; color = color; title = title; status = 0}
                | _ ->
                    Error (Exception ("unmatch column type"))
            match res with
            | Ok label -> yield label
            | Error err -> printfn "%A" err ]

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let labels: ModelCollection<Label> = Model.select user
            return! Controller.json ctx labels
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
            let! body = Controller.getJson<LabelCreateParams> ctx
            let dataset: Label = List.head (Model.create body.color body.title user)
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
