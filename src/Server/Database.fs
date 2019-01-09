module Database

open System
open Npgsql
open NpgsqlTypes

type Connection =
    { host: string
      database: string
      username: string
      password: string
      port: int
      config : string }

    override conn.ToString () =
        sprintf "Host=%s;Username=%s;Password=%s;Database=%s;Port=%d;%s"
            conn.host
            conn.username
            conn.password
            conn.database
            conn.port
            conn.config

let defaultConnection =
    { host = "localhost"
      database = "feblr"
      username = "feblr"
      password = "feblr"
      port = 5432
      config = "" }

let withHost host conn = { conn with host = host }
let withDatabase database conn = { conn with database = database }
let withUsername username conn = { conn with username = username }
let withPassword password conn = { conn with password = password }
let withPort port conn = { conn with port = port }
let withConfig config conn = { conn with config = config }

type SqlType =
    | Smallint
    | Integer
    | Bigint
    | Text
    | CharacterVaring
    | Character
    | Uuid
    | TimestampWithTimeZone
    | Json
    | Option of SqlType

type SqlData =
    | Smallint of int16
    | Integer of int
    | Bigint of int64
    | Text of string
    | CharacterVaring of string
    | Character of string
    | Uuid of Guid
    | TimestampWithTimeZone of DateTime
    | Json of string
    | Null

type Sql =
    { statement: string
      parameters: list<string * SqlData>
      columnTypes: Map<string, SqlType> }

type Row = Map<string, SqlData>

type Table = Row list

let rec parseColumn (sqlType: SqlType) (value: obj) =
    match sqlType with
    | SqlType.Smallint ->
        match value with
        | :? int16 as x -> Ok (Smallint x)
        | _ -> Error "unmatch type and value: smallint"
    | SqlType.Integer ->
        match value with
        | :? int as x -> Ok (Integer x)
        | _ -> Error "unmatch type and value: integer"
    | SqlType.Bigint ->
        match value with
        | :? int64 as x -> Ok (Bigint x)
        | _ -> Error "unmatch type and value: bigint"
    | SqlType.Character ->
        match value with
        | :? string as x -> Ok (Character x)
        | _ -> Error "unmatch type and value character"
    | SqlType.CharacterVaring ->
        match value with
        | :? string as x -> Ok (CharacterVaring x)
        | _ -> Error "unmatch type and value character varing"
    | SqlType.Text ->
        match value with
        | :? string as x -> Ok (Text x)
        | _ -> Error "unmatch type and value text"
    | SqlType.Uuid ->
        match value with
        | :? Guid as x -> Ok (Uuid x)
        | _ -> Error "unmatch type and value uuid"
    | SqlType.TimestampWithTimeZone ->
        match value with
        | :? DateTime as x -> Ok (TimestampWithTimeZone x)
        | _ -> Error "unmatch type and value timestamp"
    | SqlType.Json ->
        match value with
        | :? string as x -> Ok (SqlData.Json x)
        | _ ->
            printfn "%A" (value.GetType())
            Error "unmatch type and value json"
    | SqlType.Option sqlType ->
        match value with
        | :? System.DBNull
        | null -> Ok Null
        | _ ->
            parseColumn sqlType (box value)

let private readRow (columnTypes: Map<string, SqlType>) (reader: NpgsqlDataReader): Result<Row, exn> =
    try
        [0 .. reader.FieldCount - 1]
        |> List.fold (fun row columnIndex ->
            match row with
            | Ok row ->
                let columnName = reader.GetName(columnIndex)
                match Map.tryFind columnName columnTypes with
                | Some sqlType ->
                    let value = reader.GetValue(columnIndex)
                    match parseColumn sqlType value with
                    | Ok sqlData ->
                        Ok (Map.add columnName sqlData row)
                    | Error err ->
                        Error (Exception err)
                | None ->
                    Error (Exception ("No type definition!" + columnName))
            | Error err ->
                Error err
        ) (Ok Map.empty)
    with
    | err -> Error err


let execute (conn: Connection) (sql: Sql) : Table =
    use conn = new NpgsqlConnection(conn.ToString())
    conn.Open()

    use command = new NpgsqlCommand(sql.statement, conn)
    
    for parameter in sql.parameters do
        let paramName = fst parameter
        let paramValue = snd parameter
        let dbType, value =
            match paramValue with
            | Smallint value ->
                NpgsqlDbType.Smallint, value :> obj
            | Integer value ->
                NpgsqlDbType.Integer, value :> obj
            | Bigint value ->
                NpgsqlDbType.Bigint, value :> obj
            | Text value ->
                NpgsqlDbType.Text, value :> obj
            | CharacterVaring value ->
                NpgsqlDbType.Char, value :> obj
            | Character value ->
                NpgsqlDbType.Char, value :> obj
            | Uuid value ->
                NpgsqlDbType.Uuid, value :> obj
            | Json value ->
                NpgsqlDbType.Json, value :> obj
            | TimestampWithTimeZone value ->
                NpgsqlDbType.TimestampTz, value :> obj
            | Null ->
                failwith "can not use null as column value"

        command.Parameters.AddWithValue(paramName, dbType, value) |> ignore
    use reader = command.ExecuteReader ()

    [ while reader.Read() do
        let row = readRow sql.columnTypes reader
        match row with
        | Ok row ->
            yield row
        | Error err ->
            printfn "error occoured when parsing row: %A" err
            () ]