module DatasetCreate

open Shared.Model
open Elmish
open Elmish.Browser.Navigation
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.React
open Fable.PowerPack.Fetch
open Thoth.Json

type Model =
    { creating: bool
      datasetCreateParams: DatasetCreateParams }

type Msg =
    | ChangeTitle of string
    | Create
    | CreateResponse of Result<Dataset, exn>

let init () =
    let model =
        { creating = false
          datasetCreateParams = { title = ""; } }

    model, Cmd.none

let update msg model =
    match msg with
    | ChangeTitle title ->
        let newDatasetCreateParams = { model.datasetCreateParams with title = title}
        { model with datasetCreateParams = newDatasetCreateParams }, Cmd.none
    | Create ->
        let session: Result<Session, string>  = Token.load ()
        match session with
        | Ok session ->
            let cmd =
                let authorization = sprintf "Bearer %s" session.token
                let body = Encode.toString 0 (DatasetCreateParams.Encoder model.datasetCreateParams)
                let defaultProps =
                    [ RequestProperties.Method HttpMethod.POST
                      requestHeaders
                          [ ContentType "application/json" 
                            Authorization authorization ]
                      RequestProperties.Body <| unbox body ]

                Cmd.ofPromise
                    (fun _ -> fetchAs "/api/datasets" Dataset.Decoder defaultProps)
                    ()
                    (Ok >> CreateResponse)
                    (Error >> CreateResponse)

            { model with creating = true }, cmd
        | Error _ ->
            model, Cmd.none
    | CreateResponse (Ok dataset) ->
        let datasetUrl = sprintf "/datasets/%i" dataset.id
        { model with creating = false }, Navigation.newUrl datasetUrl
    | CreateResponse (Error err) ->
        { model with creating = false}, Cmd.none


let view model dispatch =
    let submit (evt: FormEvent) =
        evt.preventDefault()

        dispatch Create

    let changeTitle (evt: FormEvent) =
        dispatch <| ChangeTitle evt.Value

    let datasetForm =
        form [ ClassName "form form--vertical"; OnSubmit submit ]
            [ div [ ClassName "form__field" ] [ input [ ClassName "form__control"; Placeholder "Input dataset title"; OnChange changeTitle; Value model.datasetCreateParams.title ] ]
              div [ ClassName "form__field" ] [ button [ ClassName "form__control button--solid button--primary"; Disabled model.creating ] [ str "Create Dataset" ] ] ] 

    let pageHeader =
        header [ classList [("flex__box", true)] ]
            [ p [ classList [("flex__item", true)] ] [ a [ Href "/datasets" ] [ str "  <  " ]; str "Datasets" ] ] 

    div [] [ pageHeader; datasetForm]
