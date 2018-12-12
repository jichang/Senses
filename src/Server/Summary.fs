module Summary

open Shared.Model
open Saturn
open Database
open Microsoft.AspNetCore.Http
open System.Security.Claims
open Thoth.Json.Net


module Model =
    let summaryTable =
        Map.ofList
            [ "user_id", SqlType.Bigint
              "labels_count", SqlType.Bigint
              "datasets_count", SqlType.Bigint ]

    let select (user: User): Summary =
        let statement = """
            SELECT
                @user_id as user_id,
                (SELECT count(*) from senses.datasets where user_id = @user_id) as datasets_count,
                (SELECT count(*) from senses.labels where user_id = @user_id) as labels_count
        """

        let sql =
            { statement = statement
              parameters = [("user_id", Bigint user.id)]
              columnTypes = summaryTable }
        let rows = Database.execute Database.defaultConnection sql
        match rows.Length with
        | 1 ->
          let row = rows.[0]
          let datasetsColumn = row.Item "datasets_count"
          let labelsColumn = row.Item "labels_count"
          match datasetsColumn, labelsColumn with
          | Bigint datasetsCount, Bigint labelsCount ->
              { datasetsCount = datasetsCount
                labelsCount = labelsCount }
          | _ ->
            { datasetsCount = 0L
              labelsCount = 0L }
        | _ ->
          { datasetsCount = 0L
            labelsCount = 0L }


module Controller =
    open FSharp.Control.Tasks.ContextInsensitive

    let indexAction (ctx: HttpContext) = task {
        let sub = ctx.User.FindFirst ClaimTypes.NameIdentifier
        match Decode.fromString User.Decoder sub.Value with
        | Ok user ->
            let summary: Summary = Model.select user
            return! Controller.json ctx summary
        | Error err ->
            printfn "error parsing claim: %A" err
            let response =
                { code = "invalid user code"}
            return! Controller.json ctx response
    }


let controller = controller {
    index Controller.indexAction
}
