module DatasetSlices

open System
open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net
open Microsoft.AspNetCore.Http.Features
open System.Threading
open System.IO


module Model =
    open FSharp.Control.Tasks.ContextInsensitive

    let slicesTable =
        Map.ofList
            [ "id", SqlType.Bigint
              "user_id", SqlType.Bigint
              "dataset_id", SqlType.Bigint
              "title", SqlType.CharacterVaring
              "status", SqlType.Integer ]

    let selectAll (user: User) (datasetId: int64): ModelCollection<DatasetSlice> =
        let columnTypes =
            Map.add "total_count" SqlType.Bigint slicesTable

        let statement = """
        SELECT id, title, status, count(*) OVER() AS total_count
        FROM senses.dataset_slices
        WHERE user_id=@user_id AND dataset_id=@dataset_id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id);("dataset_id", Bigint datasetId)]
              columnTypes = columnTypes }
        let rows = Database.execute Database.defaultConnection sql

        match List.isEmpty rows with
        | true ->
            { totalCount = 0L; items = [] }
        | false ->
            let row = List.head rows
            match row.Item "total_count" with
            | Bigint totalCount ->
                let slices =
                    [ for row in rows do
                        let res =
                            let idColumn = row.Item "id"
                            let titleColumn = row.Item "title"
                            let statusColumn = row.Item "status"
                            match idColumn, titleColumn, statusColumn with
                            | Bigint id, CharacterVaring title, Integer status ->
                                Ok { id = id; title = title; status = status}
                            | _ ->
                                Error (Exception ("unmatch column value"))

                        match res with
                        | Ok dataset -> yield dataset
                        | Error err -> printfn "%A" err ]
                { totalCount = totalCount; items = slices }
            | _ ->
                { totalCount = 0L; items = [] }

    // this better runs in the same transaction
    let create (user: User) (datasetId: int64) (title: string) (resourcesFile: IFormFile) =
        let statement = """
        INSERT INTO senses.dataset_slices(user_id, dataset_id, title)
        VALUES (@user_id, @dataset_id, @title) RETURNING id
        """
        let sql =
            { statement = statement
              parameters =
                  [ ("title", CharacterVaring title)
                    ("dataset_id", Bigint datasetId)
                    ("user_id", Bigint user.id) ]
              columnTypes = slicesTable }
        let rows = Database.execute Database.defaultConnection sql

        if rows.Length = 1 then
            let row = rows.[0]
            let idColumn = row.Item "id"
            match idColumn with
            | Bigint id ->
                use stream = resourcesFile.OpenReadStream()
                use reader = new StreamReader(stream)
                while reader.EndOfStream = false do
                    let line = reader.ReadLine()
                    let json = Decode.fromString ResourceFileLine.Decoder (line.Trim())
                    match json with
                    | Ok line ->
                        match Resources.Model.create user datasetId id line with
                        | Ok resourceId ->
                            printfn "%A" resourceId
                        | Error err ->
                            printfn "%A" err
                    | Error error ->
                        printfn "%A" error

                Ok { id = id; title = title; status = 0 }
            | _ ->
                Error (Exception "unmatch column type")
        else
            Error (Exception "unexpected error")

    let selectOne (user: User) (datasetId: int64) (id: int64) : Result<DatasetSlice, exn> =
        let statement = """
        SELECT id, title, status
        FROM senses.dataset_slices
        WHERE user_id=@user_id AND dataset_id=@dataset_id AND id=@id
        """
        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id); ("dataset_id", Bigint datasetId); ("id", Bigint id)]
              columnTypes = slicesTable }
        let rows = Database.execute Database.defaultConnection sql

        match rows.Length with
        | 1 ->
            let row = List.head rows
            let idColumn = row.Item "id"
            let titleColumn = row.Item "title"
            let statusColumn = row.Item "status"
            match idColumn, titleColumn, statusColumn with
            | Bigint id, CharacterVaring title, Integer status ->
                Ok { id = id; title = title; status = status}
            | _ ->
                Error (Exception ("unmatch column value"))
        | _ ->
            Error (Exception "No database found")

module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (datasetId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let slices: ModelCollection<DatasetSlice> = Model.selectAll user datasetId
            return! Controller.json ctx slices
        | Error err ->
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let createAction (datasetId: int64) (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let formFeature = ctx.Features.Get<IFormFeature>()
            let! form = formFeature.ReadFormAsync CancellationToken.None
            let resourcesFile = form.Files.[0]
            let title = (form.Item "title").[0]
            let slice = Model.create user datasetId title resourcesFile
            return! Controller.json ctx slice
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }

    let showAction (datasetId: int64) (ctx: HttpContext) (id: int64) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            match Model.selectOne user datasetId id with
            | Ok slice ->
                return! Controller.json ctx slice
            | Error _ ->
                return! Controller.text ctx ""
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }


let controller (datasetId: int64) = controller {
    subController "/resources" (Resources.controller datasetId)

    index (Controller.indexAction datasetId)
    create (Controller.createAction datasetId)
    show (Controller.showAction datasetId)
}
