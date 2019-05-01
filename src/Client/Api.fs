module Api

open Fable.Core
open Shared.Model
open Thoth.Json
open Fetch

let internal decode (text: string) ((decoder: Decoder<'T>)) : 'T =
    match Decode.fromString decoder text with
    | Ok t -> t
    | Error str -> failwith str

let fetchAs<'T> (url: string) (init: RequestProperties list) (decoder: Decoder<'T>)  : JS.Promise<'T> =
    fetch url init
    |> Promise.bind (fun response -> response.text())
    |> Promise.map (fun text -> decode text decoder)

module Dataset =
    let create (token: string) (createParams: DatasetCreateParams) =
        let authorization = sprintf "Bearer %s" token
        let body = Encode.toString 0 (DatasetCreateParams.Encoder createParams)
        let headers =
          [ ContentType "application/json" 
            Authorization authorization ]
        let defaultProps =
            [ RequestProperties.Method HttpMethod.POST
              requestHeaders headers
              RequestProperties.Body <| unbox body ]

        fetchAs "/api/datasets" defaultProps Dataset.Decoder